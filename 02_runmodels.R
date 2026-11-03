# =============================================================================
# 02_runmodels.R | Rethinking Nearshoring | PPML-DiD, audit, sensitivity
# Input: trade_data_integrated.rds, dist_cepii.xlsx, control_candidates_provisional.csv
# Output: model_results/ (never overwrites raw archives)
# Run from project folder: source('02_runmodels.R', encoding='UTF-8')
# NOTE: 31 controls are EX-ANTE PROVISIONAL, not yet final due to ROU HS71/93.
# NOTE: observed trade cells ONLY; missing cells are NOT assumed to be zero.
# NOTE: HS2 H4/H5/H6 vintages must be checked before causal interpretation.
# =============================================================================

ibr_project_dir <- function(script_name) {
  frames <- sys.frames()
  paths <- vapply(frames, function(f) {
    x <- f$ofile
    if (is.null(x) || !length(x)) "" else as.character(x[1])
  }, character(1))
  paths <- paths[nzchar(paths)]
  if (length(paths)) {
    p <- normalizePath(dirname(tail(paths, 1)), winslash="/", mustWork=TRUE)
    if (file.exists(file.path(p, script_name))) return(p)
  }
  p <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
  if (!file.exists(file.path(p, script_name))) stop("Set working directory to Article_IBR_FINAL or source by full path.")
  p
}

# =============================================================================
# H1: POST-2020 DISTANCE SENSITIVITY IN EU IMPORTS
# No API calls, no imputation, no raw archive modifications.
# Run: options(ibr.model.mode='h1'); source('.../02_runmodels.R')
# Later H2: options(ibr.model.mode='h2'); source('.../02_runmodels.R')
# =============================================================================
ibr_model_mode <- getOption('ibr.model.mode','h1')
if (!ibr_model_mode %in% c('h1','h2','h1_checks','h1_final_checks','h2_sector','h2_checks','h2_distance','h2_validation','h3_turkey','final_all')) stop('ibr.model.mode must be h1, h1_checks, h1_final_checks, h2, h2_sector or h2_checks or h2_distance')
if (identical(ibr_model_mode,'h1')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir,'model_results','H1')
  for (p in c('data.table','fixest','readxl','countrycode')) {
    if (!requireNamespace(p,quietly=TRUE))
      stop('Install required package: ',p)
  }
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  distance_path <- file.path(project_dir,'dist_cepii.xlsx')
  if (!file.exists(input_path) || !file.exists(distance_path))
    stop('Missing integrated trade archive or dist_cepii.xlsx')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
            'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
            'POL','PRT','ROU','SVK','SVN','ESP','SWE')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  raw <- data.table::as.data.table(readRDS(input_path))
  if (nrow(raw)!=1499841L) stop('Unexpected integrated record count')
  fields <- c('reporter_iso','partner_iso','ref_year','cmd_code',
              'flow_code','aggr_level','primary_value','classification_code')
  if (!all(fields %in% names(raw))) stop('Missing trade columns')
  d <- raw[,.(importer=toupper(trimws(as.character(reporter_iso))),
             supplier=toupper(trimws(as.character(partner_iso))),
             year=as.integer(as.character(ref_year)),
             hs2=sprintf('%02d',as.integer(as.character(cmd_code))),
             flow=as.character(flow_code),
             aggr=as.integer(as.character(aggr_level)),
             value=as.numeric(as.character(primary_value)),
             vintage=as.character(classification_code))]
  if (anyNA(d[,.(importer,supplier,year,hs2,flow,aggr,value,vintage)]))
    stop('Missing trade fields')
  if (any(!is.finite(d$value)) || any(d$value<0)) stop('Invalid trade values')
  d <- d[importer %in% EU27 & year %in% 2015:2025 &
         hs2 %in% HS68 & flow=='M' & aggr==2L]
  valid_iso <- unique(c(stats::na.omit(countrycode::codelist$iso3c),'XKX'))
  d <- d[supplier %in% valid_iso & !supplier %in% EU27 & supplier!=importer]
  if (anyDuplicated(d,by=c('importer','supplier','year','hs2')))
    stop('Duplicate model keys')
  vintage_expected <- c('H4','H4','H5','H5','H5','H5','H5','H6','H6','H6','H6')
  if (any(d$vintage != vintage_expected[d$year-2014L]))
    stop('Unexpected HS vintage; review data')

  ce <- data.table::as.data.table(readxl::read_excel(distance_path))
  if (!all(c('iso_o','iso_d','distw') %in% names(ce)))
    stop('CEPII must contain iso_o, iso_d, distw')
  ce <- ce[,.(iso_o=toupper(trimws(as.character(iso_o))),
             iso_d=toupper(trimws(as.character(iso_d))),
             distw=as.numeric(distw))]
  # Direction: origin is supplier, destination is EU importer.
  # Historical code bridges are used ONLY for the CEPII lookup.
  bridge <- function(x) {
    x[x=='ROU'] <- 'ROM'
    x[x=='SRB'] <- 'YUG'
    x
  }
  d[, ce_origin := bridge(supplier)]
  d[, ce_destination := bridge(importer)]
  pair <- unique(d[,.(supplier,importer,ce_origin,ce_destination)])
  ce <- ce[!is.na(iso_o) & !is.na(iso_d)]
  if (anyDuplicated(ce,by=c('iso_o','iso_d')))
    stop('Duplicate directed CEPII pairs; investigate before estimation')
  pair <- merge(pair,ce,by.x=c('ce_origin','ce_destination'),
                by.y=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  pair[, valid_distance := !is.na(distw) & is.finite(distw) & distw>0]
  # Complete EU27 CEPII coverage is a transparent H1 sample restriction,
  # NOT an assertion that excluded countries have zero trade.
  all_pairs <- data.table::CJ(importer=EU27,supplier=sort(unique(d$supplier)))
  all_pairs[, ce_origin := bridge(supplier)]
  all_pairs[, ce_destination := bridge(importer)]
  all_pairs <- merge(all_pairs,ce,by.x=c('ce_origin','ce_destination'),
                     by.y=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  all_pairs[, valid_distance := !is.na(distw) & is.finite(distw) & distw>0]
  complete <- all_pairs[,.(all_27=all(valid_distance)),by=supplier]
  excluded <- complete[all_27==FALSE,supplier]
  selected <- complete[all_27==TRUE,supplier]
  if (!length(selected)) stop('No suppliers with complete EU27 distances')
  d <- d[supplier %in% selected]
  pair <- pair[valid_distance==TRUE & supplier %in% selected,
               .(importer,supplier,distw)]
  d <- merge(d,pair,by=c('importer','supplier'),all.x=TRUE,sort=FALSE)
  if (anyNA(d$distw) || any(d$distw<=0)) stop('Unmatched distance in H1 sample')
  d[, logdist := log(distw)]
  d[, post := as.integer(year>=2020L)]
  d[, post_logdist := post*logdist]
  if (!dir.exists(output_dir)) dir.create(output_dir,recursive=TRUE)
  data.table::fwrite(all_pairs[,.(importer,supplier,distw,valid_distance)],
    file.path(output_dir,'H1_all_CEPII_pairs_audit.csv'))
  data.table::fwrite(complete,file.path(output_dir,'H1_supplier_eligibility.csv'))
  coverage <- d[,.(records=.N,importers=data.table::uniqueN(importer),
                    suppliers=data.table::uniqueN(supplier)),by=year]
  data.table::fwrite(coverage,file.path(output_dir,'H1_year_coverage.csv'))
  writeLines(c('H1 SAMPLE AUDIT',
    paste('Input rows:',nrow(raw)),
    paste('Estimation rows before FE drops:',nrow(d)),
    paste('Eligible suppliers:',length(selected)),
    paste('Excluded suppliers with incomplete EU27 distances:',length(excluded)),
    paste('Excluded ISO3:',paste(excluded,collapse=', ')),
    'CEPII direction: supplier iso_o -> importer iso_d',
    'ROU -> ROM and SRB -> YUG are distance lookup bridges only.',
    'No missing trade observations were set to zero.',
    'HS H4/H5/H6 are not harmonized; 2022 onward needs sensitivity analysis.',
    'Distance coverage restriction may affect representativeness.',
    'This is an associational test; 2020 is not a uniquely identified pandemic shock.'),
    file.path(output_dir,'H1_sample_audit.txt'))

  cat('H1 sample:',nrow(d),'rows;',length(selected),'suppliers.\n')
  cat('Estimating PPML H1 main specification...\n')
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  formula_h1 <- value ~ post_logdist |
    importer^supplier^hs2 + importer^hs2^year
  fit <- fixest::fepois(formula_h1,data=d,vcov=~supplier,
                       notes=TRUE,warn=TRUE)
  tab <- fixest::coeftable(fit)
  if (!'post_logdist' %in% rownames(tab)) stop('H1 interaction not identified')
  out <- data.frame(term=rownames(tab),estimate=tab[,1],se=tab[,2],
                    z=tab[,3],p_value=tab[,4],row.names=NULL)
  data.table::fwrite(out,file.path(output_dir,'H1_main_PPML.csv'))
  writeLines(capture.output(summary(fit)),
             file.path(output_dir,'H1_main_PPML_summary.txt'))
  rm(fit); invisible(gc(verbose=FALSE))
  cat('Estimating H1 annual distance-interaction event study...\n')
  event_fit <- fixest::fepois(
    value ~ i(year,logdist,ref=2019) |
      importer^supplier^hs2 + importer^hs2^year,
    data=d,vcov=~supplier,notes=TRUE,warn=TRUE)
  ev <- fixest::coeftable(event_fit)
  data.table::fwrite(data.frame(term=rownames(ev),estimate=ev[,1],
    se=ev[,2],z=ev[,3],p_value=ev[,4],row.names=NULL),
    file.path(output_dir,'H1_event_study.csv'))
  rm(event_fit); invisible(gc(verbose=FALSE))
  cat('H1 completed. Review:',output_dir,'\n')
  print(out)
}

# =============================================================================
# H2: TURKEY vs 31 PRE-2020-SELECTED FARTHER NON-EU SUPPLIERS
# =============================================================================
if (identical(ibr_model_mode,'h2')) {
project_dir <- ibr_project_dir("02_runmodels.R")
output_dir <- file.path(project_dir, 'model_results')
if (!dir.exists(output_dir)) dir.create(output_dir, recursive=TRUE)
for (p in c('data.table','fixest')) {
  if (!requireNamespace(p, quietly=TRUE))
    stop('Missing R package: ',p,'. Install with install.packages("',p,'")')
}

input_path <- file.path(project_dir,'trade_data_integrated.rds')
controls_path <- file.path(project_dir,'control_candidates_provisional.csv')
if (!file.exists(input_path) || !file.exists(controls_path))
  stop('Missing trade_data_integrated.rds or control_candidates_provisional.csv')
controls_table <- utils::read.csv(controls_path,stringsAsFactors=FALSE)
if (!all(c('partner_iso','provisional_control','eligible_EU27','farther_than_TUR') %in% names(controls_table)))
  stop('Control selection file is missing required columns')
controls <- sort(unique(as.character(controls_table$partner_iso[
  controls_table$provisional_control %in% TRUE &
  controls_table$eligible_EU27 %in% TRUE &
  controls_table$farther_than_TUR %in% TRUE])))
if (length(controls)!=31L || 'TUR' %in% controls)
  stop('Expected exactly 31 ex-ante controls excluding TUR; review selection before modeling')

EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
          'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
          'POL','PRT','ROU','SVK','SVN','ESP','SWE')
HS68 <- sprintf('%02d',c(28:76,78:96))
raw <- data.table::as.data.table(readRDS(input_path))
needed <- c('reporter_iso','partner_iso','ref_year','cmd_code','flow_code',
            'aggr_level','classification_code','primary_value')
if (!all(needed %in% names(raw))) stop('Extended dataset missing required fields')
if (nrow(raw)!=1499841L) stop('Integrated dataset row count differs from validated 1499841')

# Keep original trade amounts, do not aggregate duplicates or fill absent cells.
d <- raw[, .(
  importer=toupper(trimws(as.character(reporter_iso))),
  supplier=toupper(trimws(as.character(partner_iso))),
  year=suppressWarnings(as.integer(as.character(ref_year))),
  hs2=sprintf('%02d',suppressWarnings(as.integer(as.character(cmd_code)))),
  flow=as.character(flow_code),
  aggr=suppressWarnings(as.integer(as.character(aggr_level))),
  hs_vintage=as.character(classification_code),
  value=suppressWarnings(as.numeric(as.character(primary_value)))
)]
d <- d[importer %in% EU27 & supplier %in% c('TUR',controls) &
       year %in% 2015:2025 & hs2 %in% HS68 & flow=='M' & aggr==2L]
if (!nrow(d) || anyNA(d[,.(importer,supplier,year,hs2,hs_vintage,value)]) ||
    any(!is.finite(d$value)) || any(d$value<0))
  stop('Invalid or missing model fields; no estimation performed')
if (anyDuplicated(d,by=c('importer','supplier','year','hs2')))
  stop('Duplicate importer-supplier-year-HS2 records; no estimation performed')
if (!setequal(unique(d$supplier),c('TUR',controls)))
  stop('Some pre-selected controls have no data in the extended dataset')
if (!setequal(unique(d$importer),EU27)) stop('Some EU importers are missing')
if (!setequal(unique(d$year),2015:2025)) stop('Some years are missing')

# Each supplier must appear in each year; this is NOT proof of complete HS2 cells.
coverage <- d[,.(records=.N, chapters=data.table::uniqueN(hs2),
                importers=data.table::uniqueN(importer),
                total_usd=sum(value)),by=.(supplier,year)]
data.table::setorder(coverage,supplier,year)
data.table::fwrite(coverage,file.path(output_dir,'supplier_year_coverage.csv'))
if (nrow(coverage)!=32L*11L || any(coverage$records==0L))
  stop('At least one supplier-year has no observations; inspect supplier_year_coverage.csv')

# Diagnostic: expected cells vs observed cells, without zero imputation.
expected_n <- 27L*32L*length(HS68)*11L
missing_n <- expected_n-nrow(d)
if (missing_n<0) stop('Observed cells exceed expected panel grid')
missing_rou <- d[importer=='ROU' & year %in% 2015:2019 & hs2 %in% c('71','93')]
if (nrow(missing_rou)>0L)
  warning('Romania HS71/93 pre-period has observed records: revisit prior missingness audit')

vintage <- d[,.(records=.N,chapters=data.table::uniqueN(hs2)),by=.(year,hs_vintage)]
data.table::setorder(vintage,year,hs_vintage)
data.table::fwrite(vintage,file.path(output_dir,'hs_vintage_audit.csv'))
expected_vintage <- c('2015'='H4','2016'='H4','2017'='H5','2018'='H5',
                      '2019'='H5','2020'='H5','2021'='H5','2022'='H6',
                      '2023'='H6','2024'='H6','2025'='H6')
if (any(d$hs_vintage != unname(expected_vintage[as.character(d$year)])))
  stop('Unexpected HS classification vintage: inspect hs_vintage_audit.csv')

# Define model sample and treatment. Event and post years are prespecified.
d[, tur := as.integer(supplier=='TUR')]
d[, post := as.integer(year>=2020L)]
d[, did := tur*post]
d[, year_factor := factor(year)]
d[, year_num := year-2015L]
# Absorbed FEs: importer x supplier x HS2, importer x HS2 x year.
# The second FE absorbs all importer-product-specific year shocks.
# Supplier-clustered SE; 32 supplier clusters. Conditional parallel trends
# remain an identifying assumption, not established by these models.

# Record provenance and sample restrictions before any estimation.
audit <- c(
  'RETHINKING NEARSHORING | MODEL INPUT AUDIT',
  paste('Run:',format(Sys.time(),'%Y-%m-%d %H:%M:%S %Z')),
  paste('Source:',input_path),
  paste('Controls:',paste(controls,collapse=', ')),
  paste('Number of controls:',length(controls)),
  paste('Observed model records:',nrow(d)),
  paste('Expected full grid (not imputed):',expected_n),
  paste('Absent cells (NOT set to zero):',missing_n),
  paste('Zero-valued OBSERVED cells:',sum(d$value==0)),
  paste('ROU 2015-2019 HS71/93 observed records:',nrow(missing_rou)),
  'Selection remains PROVISIONAL: ROU 2015-2019 HS71/HS93 unavailable.',
  'AB26 stability of screening does not prove absence of selection bias.',
  'HS2 vintages H4/H5/H6; no concordance applied. Interpret post-2022 carefully.',
  'No supplier removed based on estimated treatment effect or pretrends.',
  'Observed-only PPML: absent trade cells not imputed; extensive margin may be biased.',
  'Post begins 2020, including pandemic years; estimates are associations unless',
  'parallel trends and other identification assumptions are justified.',
  'Specification: importer-supplier-HS2 FE + importer-HS2-year FE.',
  'SEs clustered by supplier; supplementary clustering by importer provided.'
)
writeLines(audit,file.path(output_dir,'model_input_audit.txt'))

# Avoid huge model objects on disk. Estimate sequentially; capture coefficients.
fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
model_formula <- value ~ did | importer^supplier^hs2 + importer^hs2^year
extract_did <- function(m,label,cluster_name) {
  co <- fixest::coeftable(m)
  if (!'did' %in% rownames(co))
    stop('Treatment coefficient not identified in ',label)
  b <- unname(co['did',1]); se <- unname(co['did',2]); pv <- unname(co['did',4])
  data.frame(model=label,cluster=cluster_name,estimate_log=b,se_log=se,
             effect_pct=100*expm1(b),ci_low_pct=100*expm1(b-1.96*se),
             ci_high_pct=100*expm1(b+1.96*se),p_value=pv,
             n_obs=stats::nobs(m),stringsAsFactors=FALSE)
}
results <- list()
fit_did <- function(z,label,cluster='supplier') {
  if (!nrow(z)) stop('Empty sample for ',label)
  cat('Fitting:',label,'| rows:',nrow(z),'\n')
  m <- fixest::fepois(model_formula,data=z,
                     vcov=stats::as.formula(paste0('~',cluster)),
                     notes=TRUE,warn=TRUE)
  out <- extract_did(m,label,cluster)
  rm(m); invisible(gc(verbose=FALSE))
  out
}

# Main specification and prespecified robustness checks.
results[[1]] <- fit_did(d,'main_EU27_supplier_cluster')
results[[2]] <- fit_did(d,'main_EU27_importer_cluster','importer')
results[[3]] <- fit_did(d[importer!='ROU'],'exclude_Romania')
results[[4]] <- fit_did(d[!hs2 %in% c('71','93')],'exclude_HS71_HS93')
results[[5]] <- fit_did(d[!year %in% c(2020L,2021L)],'exclude_2020_2021')
results[[6]] <- fit_did(d[year<=2021L],'pre_H6_only_2015_2021')

# A Turkey-specific linear trend is a sensitivity specification, not a cure
# for non-parallel pre-trends. The did estimate changes interpretation.
cat('Fitting: turkey_linear_trend_sensitivity\n')
trend_model <- fixest::fepois(
  value ~ did + I(tur*year_num) |
    importer^supplier^hs2 + importer^hs2^year,
  data=d,vcov=~supplier,notes=TRUE,warn=TRUE)
results[[7]] <- extract_did(trend_model,'turkey_linear_trend_sensitivity','supplier')
rm(trend_model); invisible(gc(verbose=FALSE))

result_table <- do.call(rbind,results)
utils::write.csv(result_table,file.path(output_dir,'ppml_did_results.csv'),row.names=FALSE)

# Pre-period trend diagnostic: 2015-2019 only. One slope difference is tested.
# Not used for screening controls. Does not establish parallel trends.
cat('Fitting: preperiod_trend_diagnostic\n')
pre_d <- d[year<=2019L]
pre_model <- fixest::fepois(
  value ~ I(tur*year_num) |
    importer^supplier^hs2 + importer^hs2^year,
  data=pre_d,vcov=~supplier,notes=TRUE,warn=TRUE)
pre_co <- fixest::coeftable(pre_model)
pre_out <- data.frame(term=rownames(pre_co),estimate=pre_co[,1],
                      se=pre_co[,2],p_value=pre_co[,4],row.names=NULL)
utils::write.csv(pre_out,file.path(output_dir,'preperiod_trend_test.csv'),row.names=FALSE)
rm(pre_model); invisible(gc(verbose=FALSE))

# Year-by-year relative treatment coefficients, reference year 2019.
# Not a staggered-adoption design. Pre-period event estimates are diagnostics.
cat('Fitting: event_study\n')
event_model <- fixest::fepois(
  value ~ i(year,tur,ref=2019) |
    importer^supplier^hs2 + importer^hs2^year,
  data=d,vcov=~supplier,notes=TRUE,warn=TRUE)
ev_co <- fixest::coeftable(event_model)
ev <- data.frame(term=rownames(ev_co),estimate_log=ev_co[,1],
                 se_log=ev_co[,2],p_value=ev_co[,4],row.names=NULL)
ev$effect_pct <- 100*expm1(ev$estimate_log)
ev$ci_low_pct <- 100*expm1(ev$estimate_log-1.96*ev$se_log)
ev$ci_high_pct <- 100*expm1(ev$estimate_log+1.96*ev$se_log)
utils::write.csv(ev,file.path(output_dir,'event_study_coefficients.csv'),row.names=FALSE)
rm(event_model); invisible(gc(verbose=FALSE))

cat('\nMODELING COMPLETED. Review outputs in:',output_dir,'\n')
cat('Observed records:',nrow(d),'| missing grid cells:',missing_n,'\n')
cat('IMPORTANT: Selection remains provisional; no causal conclusion yet.\n')
print(result_table[,c('model','effect_pct','ci_low_pct','ci_high_pct','p_value','n_obs')],row.names=FALSE)

}


# =============================================================================
# H1 CHECKS: joint pretrend, HS vintage sensitivity, sample and coercion audits
# Run: options(ibr.model.mode='h1_checks'); source('.../02_runmodels.R')
# No downloads; no modification of original H1 estimates or raw archives.
# =============================================================================
if (identical(ibr_model_mode, 'h1_checks')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir,'model_results','H1','checks')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  for (p in c('data.table','fixest','readxl','countrycode'))
    if (!requireNamespace(p,quietly=TRUE)) stop('Required R package missing: ',p)
  library(data.table)
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  distance_path <- file.path(project_dir,'dist_cepii.xlsx')
  if (!file.exists(input_path) || !file.exists(distance_path)) stop('Missing H1 input')
  raw <- as.data.table(readRDS(input_path))
  if (nrow(raw)!=1499841L) stop('Unexpected integrated input row count')
  # Identify source fields that would fail numeric conversion; never silently fill.
  numeric_fields <- c('ref_year','cmd_code','aggr_level','primary_value')
  coercion <- rbindlist(lapply(numeric_fields,function(f) {
    x <- as.character(raw[[f]])
    n <- suppressWarnings(as.numeric(x))
    data.table(field=f,source_missing=sum(is.na(x)),
      conversion_new_NA=sum(!is.na(x) & is.na(n)),
      example_bad=paste(head(unique(x[!is.na(x) & is.na(n)]),5),collapse=' | '))
  }))
  fwrite(coercion,file.path(output_dir,'H1_coercion_audit.csv'))
  if (any(coercion$conversion_new_NA>0))
    warning('Non-numeric source values found; see H1_coercion_audit.csv')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
    'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
    'POL','PRT','ROU','SVK','SVN','ESP','SWE')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  d <- raw[,.(importer=toupper(trimws(as.character(reporter_iso))),
    supplier=toupper(trimws(as.character(partner_iso))),
    year=suppressWarnings(as.integer(as.character(ref_year))),
    hs2=sprintf('%02d',suppressWarnings(as.integer(as.character(cmd_code)))),
    flow=as.character(flow_code),
    aggr=suppressWarnings(as.integer(as.character(aggr_level))),
    value=suppressWarnings(as.numeric(as.character(primary_value))),
    vintage=as.character(classification_code))]
  d <- d[importer %in% EU27 & year %in% 2015:2025 & hs2 %in% HS68 &
    flow=='M' & aggr==2L]
  valid_iso <- unique(c(stats::na.omit(countrycode::codelist$iso3c),'XKX'))
  d <- d[supplier %in% valid_iso & !supplier %in% EU27 & supplier!=importer]
  if (anyNA(d[,.(importer,supplier,year,hs2,value,vintage)]) ||
      any(!is.finite(d$value)) || any(d$value<0) ||
      anyDuplicated(d,by=c('importer','supplier','year','hs2')))
    stop('Invalid model sample')
  ce <- as.data.table(readxl::read_excel(distance_path))
  if (!all(c('iso_o','iso_d','distw') %in% names(ce))) stop('Invalid CEPII columns')
  ce <- ce[,.(iso_o=toupper(trimws(as.character(iso_o))),
    iso_d=toupper(trimws(as.character(iso_d))),
    distw=suppressWarnings(as.numeric(as.character(distw))))]
  if (anyDuplicated(ce,by=c('iso_o','iso_d'))) stop('Duplicate CEPII pair')
  bridge <- function(x) { x[x=='ROU'] <- 'ROM'; x[x=='SRB'] <- 'YUG'; x }
  pairs <- CJ(importer=EU27,supplier=sort(unique(d$supplier)))
  pairs[,`:=`(iso_o=bridge(supplier),iso_d=bridge(importer))]
  pairs <- merge(pairs,ce,by=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  pairs[,valid:=!is.na(distw) & is.finite(distw) & distw>0]
  eligibility <- pairs[,.(eligible=all(valid)),by=supplier]
  selected <- eligibility[eligible==TRUE,supplier]
  pairs <- pairs[valid==TRUE & supplier %in% selected,
    .(importer,supplier,distw)]
  d <- merge(d[supplier %in% selected],pairs,by=c('importer','supplier'),
    all.x=TRUE,sort=FALSE)
  if (anyNA(d$distw)) stop('Distance mismatch')
  d[,`:=`(logdist=log(distw),post_logdist=as.integer(year>=2020)*log(distw))]
  year_supplier <- d[,.(rows=.N,importers=uniqueN(importer),
    hs2=uniqueN(hs2),value_usd=sum(value)),by=.(supplier,year)]
  fwrite(year_supplier,file.path(output_dir,'H1_supplier_year_coverage.csv'))
  years_by_supplier <- d[,.(n_years=uniqueN(year),
    pre_years=uniqueN(year[year<=2019]),post_years=uniqueN(year[year>=2020])),by=supplier]
  fwrite(years_by_supplier,file.path(output_dir,'H1_supplier_panel_balance.csv'))
  missing_years <- CJ(supplier=sort(unique(d$supplier)),year=2015:2025)[
    !unique(d[,.(supplier,year)]),on=.(supplier,year)]
  fwrite(missing_years,file.path(output_dir,'H1_supplier_missing_years.csv'))
  chapter_coverage <- d[,.(observed_hs2=uniqueN(hs2),rows=.N),
    by=.(importer,year)]
  chapter_coverage[,missing_hs2_count:=length(HS68)-observed_hs2]
  fwrite(chapter_coverage,file.path(output_dir,'H1_importer_year_HS_coverage.csv'))
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  fe <- value ~ post_logdist | importer^supplier^hs2 + importer^hs2^year
  fit_one <- function(z,label) {
    cat('H1 sensitivity:',label,'| rows:',nrow(z),'\n')
    m <- fixest::fepois(fe,data=z,vcov=~supplier,notes=TRUE,warn=TRUE)
    ct <- fixest::coeftable(m)
    if (!'post_logdist' %in% rownames(ct)) stop('Unidentified H1: ',label)
    out <- data.table(specification=label,estimate=unname(ct['post_logdist',1]),
      se=unname(ct['post_logdist',2]),p_value=unname(ct['post_logdist',4]),
      n_input=nrow(z),n_estimated=stats::nobs(m),
      suppliers=uniqueN(z$supplier))
    rm(m); invisible(gc(verbose=FALSE)); out
  }
  # Full period is a replication check; the pre-H6 sample is 2015-2021.
  res <- rbindlist(list(
    fit_one(d,'replicate_main_2015_2025'),
    fit_one(d[year<=2021],'pre_H6_2015_2021'),
    fit_one(d[year<=2021 & importer!='ROU'],'pre_H6_exclude_Romania'),
    fit_one(d[!hs2 %in% c('71','93')],'exclude_HS71_HS93'),
    fit_one(d[importer!='ROU'],'exclude_Romania_full_period')),
    fill=TRUE)
  fwrite(res,file.path(output_dir,'H1_sensitivity_results.csv'))
  cat('H1 joint pretrend test (2015-2018 relative to 2019)...\n')
  m_event <- fixest::fepois(value ~ i(year,logdist,ref=2019) |
    importer^supplier^hs2 + importer^hs2^year,
    data=d,vcov=~supplier,notes=TRUE,warn=TRUE)
  names_pre <- grep('^year::201[5-8]:logdist$',names(stats::coef(m_event)),value=TRUE)
  if (length(names_pre)!=4L) stop('Unexpected pretrend term names: ',paste(names_pre,collapse=', '))
  w <- fixest::wald(m_event,keep='^year::201[5-8]:logdist$',print=FALSE)
  writeLines(c('H1 joint pretrend Wald test; H0: 2015-2018 annual distance interactions jointly zero',
    paste('Terms:',paste(names_pre,collapse=', ')),
    capture.output(print(w)),
    'This is a diagnostic; rejection challenges a simple 2020-break interpretation.'),
    file.path(output_dir,'H1_joint_pretrend_Wald.txt'))
  rm(m_event); invisible(gc(verbose=FALSE))
  cat('H1 CHECKS COMPLETED. Outputs:',output_dir,'\n')
  print(res)
  cat('Supplier-year gaps:',nrow(missing_years),'\n')
  print(w)
}


# =============================================================================
# H1 FINAL CHECKS: balanced supplier years and pre-existing distance trend
# Run: options(ibr.model.mode='h1_final_checks'); source('.../02_runmodels.R')
# Outputs isolated under model_results/H1/final_checks; no data downloads.
# =============================================================================
if (identical(ibr_model_mode,'h1_final_checks')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir,'model_results','H1','final_checks')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  for (p in c('data.table','fixest','readxl','countrycode'))
    if (!requireNamespace(p,quietly=TRUE)) stop('Required R package missing: ',p)
  library(data.table)
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  distance_path <- file.path(project_dir,'dist_cepii.xlsx')
  if (!file.exists(input_path) || !file.exists(distance_path)) stop('Missing input file')
  raw <- as.data.table(readRDS(input_path))
  if (nrow(raw)!=1499841L) stop('Unexpected integrated row count')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
    'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
    'POL','PRT','ROU','SVK','SVN','ESP','SWE')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  d <- raw[,.(importer=toupper(trimws(as.character(reporter_iso))),
    supplier=toupper(trimws(as.character(partner_iso))),
    year=suppressWarnings(as.integer(as.character(ref_year))),
    hs2=sprintf('%02d',suppressWarnings(as.integer(as.character(cmd_code)))),
    flow=as.character(flow_code),
    aggr=suppressWarnings(as.integer(as.character(aggr_level))),
    value=suppressWarnings(as.numeric(as.character(primary_value))),
    vintage=as.character(classification_code))]
  d <- d[importer %in% EU27 & year %in% 2015:2025 & hs2 %in% HS68 &
    flow=='M' & aggr==2L]
  valid_iso <- unique(c(stats::na.omit(countrycode::codelist$iso3c),'XKX'))
  d <- d[supplier %in% valid_iso & !supplier %in% EU27 & supplier!=importer]
  if (anyNA(d[,.(importer,supplier,year,hs2,value,vintage)]) ||
      any(!is.finite(d$value)) || any(d$value<0) ||
      anyDuplicated(d,by=c('importer','supplier','year','hs2')))
    stop('Invalid trade sample')
  ce <- as.data.table(readxl::read_excel(distance_path))
  if (!all(c('iso_o','iso_d','distw') %in% names(ce))) stop('Invalid CEPII fields')
  ce <- ce[,.(iso_o=toupper(trimws(as.character(iso_o))),
    iso_d=toupper(trimws(as.character(iso_d))),
    distw=suppressWarnings(as.numeric(as.character(distw))))]
  if (anyDuplicated(ce,by=c('iso_o','iso_d'))) stop('Duplicate CEPII pair')
  bridge <- function(x) { x[x=='ROU'] <- 'ROM'; x[x=='SRB'] <- 'YUG'; x }
  pairs <- CJ(importer=EU27,supplier=sort(unique(d$supplier)))
  pairs[,`:=`(iso_o=bridge(supplier),iso_d=bridge(importer))]
  pairs <- merge(pairs,ce,by=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  pairs[,valid:=!is.na(distw) & is.finite(distw) & distw>0]
  eligibility <- pairs[,.(eligible=all(valid)),by=supplier]
  selected <- eligibility[eligible==TRUE,supplier]
  pairs <- pairs[valid==TRUE & supplier %in% selected,.(importer,supplier,distw)]
  d <- merge(d[supplier %in% selected],pairs,by=c('importer','supplier'),
    all.x=TRUE,sort=FALSE)
  if (anyNA(d$distw)) stop('Missing distances after merge')
  d[,`:=`(logdist=log(distw),post_logdist=as.integer(year>=2020)*log(distw),
    year_num=year-2015L)]
  years_by_supplier <- d[,.(n_years=uniqueN(year)),by=supplier]
  balanced <- years_by_supplier[n_years==11L,supplier]
  excluded <- years_by_supplier[n_years<11L]
  fwrite(excluded,file.path(output_dir,'H1_unbalanced_suppliers.csv'))
  fwrite(data.table(supplier=balanced),file.path(output_dir,'H1_balanced_suppliers.csv'))
  if (length(balanced)==0L) stop('No balanced suppliers')
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  fe_text <- 'importer^supplier^hs2 + importer^hs2^year'
  spec <- list(
    list(name='full_baseline_replication',data=d,
      formula=value ~ post_logdist | importer^supplier^hs2 + importer^hs2^year),
    list(name='balanced_suppliers_baseline',data=d[supplier %in% balanced],
      formula=value ~ post_logdist | importer^supplier^hs2 + importer^hs2^year),
    list(name='full_with_distance_linear_trend',data=d,
      formula=value ~ post_logdist + I(logdist*year_num) |
        importer^supplier^hs2 + importer^hs2^year),
    list(name='balanced_with_distance_linear_trend',data=d[supplier %in% balanced],
      formula=value ~ post_logdist + I(logdist*year_num) |
        importer^supplier^hs2 + importer^hs2^year))
  results <- list()
  for (i in seq_along(spec)) {
    x <- spec[[i]]
    cat('H1 final check:',x$name,'| input rows:',nrow(x$data),'\n')
    m <- fixest::fepois(x$formula,data=x$data,vcov=~supplier,notes=TRUE,warn=TRUE)
    ct <- fixest::coeftable(m)
    if (!'post_logdist' %in% rownames(ct)) stop('post_logdist not identified: ',x$name)
    results[[i]] <- data.table(specification=x$name,
      estimate=unname(ct['post_logdist',1]),se=unname(ct['post_logdist',2]),
      p_value=unname(ct['post_logdist',4]),
      ci_low=unname(ct['post_logdist',1]-1.96*ct['post_logdist',2]),
      ci_high=unname(ct['post_logdist',1]+1.96*ct['post_logdist',2]),
      n_input=nrow(x$data),n_estimated=stats::nobs(m),
      suppliers=uniqueN(x$data$supplier),
      trend_estimate=if ('I(logdist * year_num)' %in% rownames(ct))
        unname(ct['I(logdist * year_num)',1]) else NA_real_,
      trend_p_value=if ('I(logdist * year_num)' %in% rownames(ct))
        unname(ct['I(logdist * year_num)',4]) else NA_real_)
    rm(m); invisible(gc(verbose=FALSE))
  }
  out <- rbindlist(results,fill=TRUE)
  fwrite(out,file.path(output_dir,'H1_final_robustness_results.csv'))
  writeLines(c('H1 FINAL CHECKS AUDIT',
    paste('Input:',input_path),paste('CEPII:',distance_path),
    paste('Eligible suppliers:',length(selected)),
    paste('Balanced suppliers (observed in all 11 years):',length(balanced)),
    paste('Excluded supplier codes:',paste(excluded$supplier,collapse=', ')),
    'Balanced means supplier-year presence, NOT a balanced importer-HS2 cell panel.',
    'All specifications retain observed-only trade data; no zero imputation.',
    'Linear trend is log(distance) x (year-2015); post coefficient measures a',
    'level deviation from the imposed linear distance trend, not a causal effect.',
    'Supplier-clustered standard errors; HS vintages are not harmonized.',
    'A linear trend cannot establish parallel trends or identify a 2020 shock.'),
    file.path(output_dir,'H1_final_checks_audit.txt'))
  cat('H1 FINAL CHECKS COMPLETED. Outputs:',output_dir,'\n')
  print(out)
}


# =============================================================================
# H2 SECTOR-SPECIFIC PPML: TURKEY vs 31 FROZEN FARTHER COMPARATORS
# Run: options(ibr.model.mode='h2_sector'); source('.../02_runmodels.R')
# Does NOT run the legacy aggregate H2 mode or download data.
# =============================================================================
if (identical(ibr_model_mode, 'h2_sector')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir, 'model_results', 'H2')
  dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  for (pkg in c('data.table','fixest')) {
    if (!requireNamespace(pkg,quietly=TRUE)) stop('Install R package: ',pkg)
  }
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  controls_path <- file.path(project_dir,'control_candidates_provisional.csv')
  if (!file.exists(input_path) || !file.exists(controls_path)) stop('Missing H2 input files')
  ctrl <- data.table::fread(controls_path)
  required_ctrl <- c('partner_iso','provisional_control','eligible_EU27','farther_than_TUR')
  if (!all(required_ctrl %in% names(ctrl))) stop('Missing control-screening columns')
  controls <- sort(unique(ctrl[provisional_control==TRUE & eligible_EU27==TRUE & farther_than_TUR==TRUE, as.character(partner_iso)]))
  if (length(controls)!=31L || 'TUR' %in% controls) stop('Frozen H2 control list is not 31 non-TUR suppliers')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA','DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','ESP','SWE')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  raw <- data.table::as.data.table(readRDS(input_path))
  needed <- c('reporter_iso','partner_iso','ref_year','cmd_code','flow_code','aggr_level','classification_code','primary_value')
  if (!all(needed %in% names(raw))) stop('Missing integrated data columns')
  if (nrow(raw)!=1499841L) stop('Unexpected integrated input row count')
  d <- raw[, .(importer=toupper(trimws(as.character(reporter_iso))),
    supplier=toupper(trimws(as.character(partner_iso))),
    year=as.integer(as.character(ref_year)),
    hs2=sprintf('%02d',as.integer(as.character(cmd_code))),
    flow=as.character(flow_code),aggr=as.integer(as.character(aggr_level)),
    vintage=as.character(classification_code),value=as.numeric(as.character(primary_value)))]
  d <- d[importer %in% EU27 & supplier %in% c('TUR',controls) & year %in% 2015:2025 & hs2 %in% HS68 & flow=='M' & aggr==2L]
  if (!nrow(d) || anyNA(d[,.(importer,supplier,year,hs2,vintage,value)]) || any(!is.finite(d$value)) || any(d$value<0)) stop('Invalid H2 input')
  if (anyDuplicated(d,by=c('importer','supplier','year','hs2'))) stop('Duplicate H2 cells')
  if (!setequal(unique(d$supplier),c('TUR',controls))) stop('Missing one or more H2 suppliers')
  if (!setequal(unique(d$importer),EU27) || !setequal(unique(d$year),2015:2025)) stop('Missing EU reporter or year')
  vintage_map <- c('2015'='H4','2016'='H4','2017'='H5','2018'='H5','2019'='H5','2020'='H5','2021'='H5','2022'='H6','2023'='H6','2024'='H6','2025'='H6')
  if (any(d$vintage != unname(vintage_map[as.character(d$year)]))) stop('HS vintage mismatch')
  d[, `:=`(tur=as.integer(supplier=='TUR'),post=as.integer(year>=2020L),year_num=year-2015L)]
  d[, did := tur*post]
  data.table::fwrite(d[,.(records=.N, suppliers=data.table::uniqueN(supplier),importers=data.table::uniqueN(importer),tur_records=sum(supplier=='TUR'),zeros=sum(value==0)),by=.(hs2,year)],file.path(output_dir,'H2_sector_year_coverage.csv'))
  writeLines(c('H2 SECTOR PPML AUDIT',paste('Run:',Sys.time()),paste('Input:',input_path),paste('Observed rows:',nrow(d)),paste('Controls:',paste(controls,collapse=', ')),
    '31 controls are pre-2020 provisional; Romania HS71/93 availability remains a limitation.',
    'Observed-only trade: missing cells never filled with zeros.',
    'HS2 H4/H5/H6 not harmonized; post-2022 coefficients require caution.',
    'Each sector: importer-supplier FE + importer-year FE; supplier-clustered SE.',
    'Only one treated supplier (TUR): conventional cluster inference may be unreliable.',
    'Pretrend test: TUR-specific linear slope over 2015-2019; diagnostic only.',
    'BH adjusted p-values calculated over all estimable sectors, not only significant ones.',
    'Post coefficients are conditional associations, not proven causal nearshoring effects.'),file.path(output_dir,'H2_sector_audit.txt'))
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  results <- vector('list',length(HS68))
  get_term <- function(m, term) {
    if (is.null(m)) return(c(NA_real_,NA_real_,NA_real_))
    ct <- fixest::coeftable(m)
    if (!term %in% rownames(ct)) return(c(NA_real_,NA_real_,NA_real_))
    as.numeric(ct[term,c(1,2,4)])
  }
  for (k in seq_along(HS68)) {
    chapter <- HS68[k]
    z <- d[hs2==chapter]
    cat(sprintf('H2 HS%s (%d/%d): %d rows\n',chapter,k,length(HS68),nrow(z)))
    base <- data.table::data.table(hs2=chapter, n_input=nrow(z),n_estimated=NA_integer_,suppliers=data.table::uniqueN(z$supplier),tur_pre=sum(z$tur==1L & z$post==0L),tur_post=sum(z$tur==1L & z$post==1L),estimate=NA_real_,se=NA_real_,p_value=NA_real_,pretrend_estimate=NA_real_,pretrend_se=NA_real_,pretrend_p_value=NA_real_,status='not_estimated')
    if (!nrow(z) || !all(c(0L,1L) %in% z$tur) || !all(c(0L,1L) %in% z$post) || !base$tur_pre || !base$tur_post) {
      base$status <- 'insufficient_treatment_coverage';results[[k]] <- base;next
    }
    fit <- tryCatch(fixest::fepois(value ~ did | importer^supplier + importer^year, data=z,vcov=~supplier,notes=TRUE,warn=TRUE),error=function(e)e)
    if (inherits(fit,'error')) {
      base$status <- paste0('model_error: ',conditionMessage(fit))
    } else {
      term <- get_term(fit,'did')
      base[,`:=`(estimate=term[1],se=term[2],p_value=term[3],n_estimated=as.integer(stats::nobs(fit)),status=if (is.finite(term[1])) 'ok' else 'did_not_identified')]
    }
    rm(fit)
    pre <- z[year<=2019L]
    if (data.table::uniqueN(pre$year)>=3L && data.table::uniqueN(pre$supplier)>=2L) {
      trend <- tryCatch(fixest::fepois(value ~ I(tur*year_num) | importer^supplier + importer^year, data=pre,vcov=~supplier,notes=TRUE,warn=TRUE),error=function(e)e)
      if (!inherits(trend,'error')) {
        ct <- fixest::coeftable(trend)
        key <- grep('tur.*year_num|year_num.*tur',rownames(ct),value=TRUE)
        if (length(key)==1L) base[,`:=`(pretrend_estimate=as.numeric(ct[key,1]),pretrend_se=as.numeric(ct[key,2]),pretrend_p_value=as.numeric(ct[key,4]))]
      }
      rm(trend)
    }
    results[[k]] <- base
    data.table::fwrite(data.table::rbindlist(results[seq_len(k)],fill=TRUE),file.path(output_dir,'H2_sector_progress.csv'))
    invisible(gc(verbose=FALSE))
  }
  out <- data.table::rbindlist(results,fill=TRUE)
  out[,p_BH:=stats::p.adjust(p_value,method='BH')]
  out[,`:=`(effect_pct=100*expm1(estimate),ci_low_pct=100*expm1(estimate-1.96*se),ci_high_pct=100*expm1(estimate+1.96*se),pretrend_p_BH=stats::p.adjust(pretrend_p_value,method='BH'))]
  data.table::setorder(out,hs2)
  data.table::fwrite(out,file.path(output_dir,'H2_sector_PPML_results.csv'))
  cat('H2 SECTOR MODELS COMPLETED. Outputs:',output_dir,'\n')
  cat('Sectors:',nrow(out),'| Estimated:',sum(out$status=='ok'),'| BH p<0.05:',sum(out$p_BH<0.05,na.rm=TRUE),'\n')
  print(out[,.(hs2,estimate,p_value,p_BH,pretrend_p_value,status)])
}


# =============================================================================
# H2 ROBUSTNESS: sector-specific Turkey linear pretrend + pre-H6 sensitivity
# Run: options(ibr.model.mode='h2_checks'); source('.../02_runmodels.R')
# Results separate from earlier H1/H2 outputs; no new API requests.
# =============================================================================
if (identical(ibr_model_mode, 'h2_checks')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  out_dir <- file.path(project_dir, 'model_results', 'H2', 'checks')
  dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
  for (pkg in c('data.table','fixest')) {
    if (!requireNamespace(pkg,quietly=TRUE)) stop('Missing package: ',pkg)
  }
  dt <- data.table::as.data.table(readRDS(file.path(project_dir,'trade_data_integrated.rds')))
  controls_path <- file.path(project_dir,'control_candidates_provisional.csv')
  old_path <- file.path(project_dir,'model_results','H2','H2_sector_PPML_results.csv')
  if (!file.exists(controls_path) || !file.exists(old_path)) stop('Run H2 sector first; missing controls or baseline output')
  ctl <- data.table::fread(controls_path)
  req <- c('partner_iso','provisional_control','eligible_EU27','farther_than_TUR')
  if (!all(req %in% names(ctl))) stop('Control screening columns missing')
  controls <- sort(unique(ctl[provisional_control==TRUE & eligible_EU27==TRUE & farther_than_TUR==TRUE,as.character(partner_iso)]))
  if (length(controls)!=31L || 'TUR' %in% controls) stop('Expected exactly 31 frozen controls')
  baseline <- data.table::fread(old_path)
  hs68 <- sprintf('%02d',c(28:76,78:96))
  if (!setequal(as.character(baseline$hs2),hs68) || nrow(baseline)!=68L) stop('Baseline H2 results do not contain all 68 chapters')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA','DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','ESP','SWE')
  if (nrow(dt)!=1499841L) stop('Integrated data row count differs from audited value')
  needed <- c('reporter_iso','partner_iso','ref_year','cmd_code','flow_code','aggr_level','classification_code','primary_value')
  if (!all(needed %in% names(dt))) stop('Missing required trade columns')
  # Explicitly filter non-numeric aggregate codes before integer conversion.
  z <- dt[toupper(trimws(as.character(reporter_iso))) %in% EU27 &
          toupper(trimws(as.character(partner_iso))) %in% c('TUR',controls) &
          as.character(ref_year) %in% as.character(2015:2025) &
          as.character(cmd_code) %in% hs68 & as.character(flow_code)=='M' &
          as.character(aggr_level)=='2',
          .(importer=toupper(trimws(as.character(reporter_iso))),
            supplier=toupper(trimws(as.character(partner_iso))),
            year=as.integer(as.character(ref_year)),hs2=as.character(cmd_code),
            vintage=as.character(classification_code),
            value=as.numeric(as.character(primary_value)))]
  if (nrow(z)!=458028L) stop('H2 filtered rows differ from previous audited 458028; inspect input')
  if (anyNA(z) || any(!is.finite(z$value)) || any(z$value<0)) stop('Invalid H2 data')
  if (anyDuplicated(z,by=c('importer','supplier','year','hs2'))) stop('Duplicate trade keys')
  expected_vintage <- c('2015'='H4','2016'='H4','2017'='H5','2018'='H5','2019'='H5','2020'='H5','2021'='H5','2022'='H6','2023'='H6','2024'='H6','2025'='H6')
  if (any(z$vintage != unname(expected_vintage[as.character(z$year)]))) stop('HS vintages mismatch')
  z[, `:=`(tur=as.integer(supplier=='TUR'),post=as.integer(year>=2020L),year_num=year-2015L)]
  z[,did:=tur*post]
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  specs <- c('full_trend','preH6_baseline','preH6_trend')
  results <- vector('list',length(hs68)*length(specs))
  index <- 0L
  for (h in hs68) {
    x <- z[hs2==h]
    for (spec in specs) {
      index <- index+1L
      w <- if (startsWith(spec,'preH6')) x[year<=2021L] else x
      res <- data.table::data.table(hs2=h,specification=spec,n_input=nrow(w),
        n_estimated=NA_integer_,suppliers=data.table::uniqueN(w$supplier),
        estimate=NA_real_,se=NA_real_,p_value=NA_real_,
        trend_estimate=NA_real_,trend_p_value=NA_real_,status='not_estimated')
      if (!nrow(w) || data.table::uniqueN(w$tur)<2L || data.table::uniqueN(w$post)<2L ||
          !any(w$tur==1L & w$post==0L) || !any(w$tur==1L & w$post==1L)) {
        res$status <- 'insufficient_coverage'
      } else {
        f <- if (grepl('trend',spec))
          value ~ did + I(tur*year_num) | importer^supplier + importer^year
        else value ~ did | importer^supplier + importer^year
        cat('H2 robustness HS',h,' | ',spec,' | rows ',nrow(w),'\n',sep='')
        m <- tryCatch(fixest::fepois(f,data=w,vcov=~supplier,notes=FALSE,warn=FALSE),error=function(e)e)
        if (inherits(m,'error')) {
          res$status <- paste0('error: ',conditionMessage(m))
        } else {
          co <- fixest::coeftable(m)
          res$n_estimated <- as.integer(stats::nobs(m))
          if ('did' %in% rownames(co)) {
            res[,`:=`(estimate=as.numeric(co['did',1]),se=as.numeric(co['did',2]),
                       p_value=as.numeric(co['did',4]),status='ok')]
          } else res$status <- 'did_not_identified'
          tr <- grep('tur.*year_num|year_num.*tur',rownames(co),value=TRUE)
          if (length(tr)==1L) res[,`:=`(trend_estimate=as.numeric(co[tr,1]),trend_p_value=as.numeric(co[tr,4]))]
        }
        rm(m)
      }
      results[[index]] <- res
      data.table::fwrite(data.table::rbindlist(results[seq_len(index)],fill=TRUE),
                         file.path(out_dir,'H2_robustness_progress.csv'))
    }
    invisible(gc(verbose=FALSE))
  }
  out <- data.table::rbindlist(results,fill=TRUE)
  out[,p_BH:=stats::p.adjust(p_value,method='BH'),by=specification]
  out[,`:=`(effect_pct=100*expm1(estimate),ci_low=estimate-1.96*se,ci_high=estimate+1.96*se)]
  base <- baseline[,.(hs2=as.character(hs2),baseline_estimate=estimate,baseline_p_BH=p_BH,
                       pretrend_p_value=pretrend_p_value,pretrend_p_BH=pretrend_p_BH)]
  out <- merge(out,base,by='hs2',all.x=TRUE,sort=FALSE)
  data.table::setorder(out,hs2,specification)
  data.table::fwrite(out,file.path(out_dir,'H2_robustness_results.csv'))
  summary <- out[,.(estimable=sum(status=='ok'),positive=sum(estimate>0,na.rm=TRUE),
      positive_BH05=sum(estimate>0 & p_BH<.05,na.rm=TRUE),
      negative_BH05=sum(estimate<0 & p_BH<.05,na.rm=TRUE)),by=specification]
  data.table::fwrite(summary,file.path(out_dir,'H2_robustness_summary.csv'))
  writeLines(c('H2 ROBUSTNESS AUDIT',paste('Run:',Sys.time()),
    'All 68 HS2 sectors; 31 pre-2020-frozen controls; 2015-2025 observed trade only.',
    'full_trend: Turkey-specific linear time trend across 2015-2025.',
    'preH6_baseline: baseline specification restricted to 2015-2021.',
    'preH6_trend: both restrictions; H4/H5 vintages still differ.',
    'BH adjustments computed separately within each 68-sector specification family.',
    'No zero imputation; no sector selection based on baseline significance.',
    'Single treated supplier: supplier-clustered p-values are descriptive/screening, not reliable causal inference.',
    'Trend model changes estimand to deviation from an imposed linear Turkey trend.',
    'This code does NOT implement randomization/placebo inference; that requires a separate step.',
    'Controls remain provisional; ROU missing HS71/93; HS vintages unharmonized.'),
    file.path(out_dir,'H2_robustness_audit.txt'))
  cat('H2 ROBUSTNESS COMPLETED: ',out_dir,'\n',sep='')
  print(summary)
}


# =============================================================================
# H2 (NEW): SECTORAL HETEROGENEITY IN POST-2020 DISTANCE SENSITIVITY
# Run: options(ibr.model.mode='h2_distance'); source('.../02_runmodels.R')
# Same 182-supplier H1 universe; no Turkey treatment or control selection.
# Three pooled PPML fits: baseline, pre-2022, distance-specific trend.
# Pre-2020 trend diagnostic is a fourth, pre-period-only pooled PPML fit.
# =============================================================================
if (identical(ibr_model_mode,'h2_distance')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir,'model_results','H1')
  for (p in c('data.table','fixest','readxl','countrycode')) {
    if (!requireNamespace(p,quietly=TRUE))
      stop('Install required package: ',p)
  }
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  distance_path <- file.path(project_dir,'dist_cepii.xlsx')
  if (!file.exists(input_path) || !file.exists(distance_path))
    stop('Missing integrated trade archive or dist_cepii.xlsx')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
            'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
            'POL','PRT','ROU','SVK','SVN','ESP','SWE')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  raw <- data.table::as.data.table(readRDS(input_path))
  if (nrow(raw)!=1499841L) stop('Unexpected integrated record count')
  fields <- c('reporter_iso','partner_iso','ref_year','cmd_code',
              'flow_code','aggr_level','primary_value','classification_code')
  if (!all(fields %in% names(raw))) stop('Missing trade columns')
  d <- raw[,.(importer=toupper(trimws(as.character(reporter_iso))),
             supplier=toupper(trimws(as.character(partner_iso))),
             year=as.integer(as.character(ref_year)),
             hs2=sprintf('%02d',as.integer(as.character(cmd_code))),
             flow=as.character(flow_code),
             aggr=as.integer(as.character(aggr_level)),
             value=as.numeric(as.character(primary_value)),
             vintage=as.character(classification_code))]
  if (anyNA(d[,.(importer,supplier,year,hs2,flow,aggr,value,vintage)]))
    stop('Missing trade fields')
  if (any(!is.finite(d$value)) || any(d$value<0)) stop('Invalid trade values')
  d <- d[importer %in% EU27 & year %in% 2015:2025 &
         hs2 %in% HS68 & flow=='M' & aggr==2L]
  valid_iso <- unique(c(stats::na.omit(countrycode::codelist$iso3c),'XKX'))
  d <- d[supplier %in% valid_iso & !supplier %in% EU27 & supplier!=importer]
  if (anyDuplicated(d,by=c('importer','supplier','year','hs2')))
    stop('Duplicate model keys')
  vintage_expected <- c('H4','H4','H5','H5','H5','H5','H5','H6','H6','H6','H6')
  if (any(d$vintage != vintage_expected[d$year-2014L]))
    stop('Unexpected HS vintage; review data')

  ce <- data.table::as.data.table(readxl::read_excel(distance_path))
  if (!all(c('iso_o','iso_d','distw') %in% names(ce)))
    stop('CEPII must contain iso_o, iso_d, distw')
  ce <- ce[,.(iso_o=toupper(trimws(as.character(iso_o))),
             iso_d=toupper(trimws(as.character(iso_d))),
             distw=as.numeric(distw))]
  # Direction: origin is supplier, destination is EU importer.
  # Historical code bridges are used ONLY for the CEPII lookup.
  bridge <- function(x) {
    x[x=='ROU'] <- 'ROM'
    x[x=='SRB'] <- 'YUG'
    x
  }
  d[, ce_origin := bridge(supplier)]
  d[, ce_destination := bridge(importer)]
  pair <- unique(d[,.(supplier,importer,ce_origin,ce_destination)])
  ce <- ce[!is.na(iso_o) & !is.na(iso_d)]
  if (anyDuplicated(ce,by=c('iso_o','iso_d')))
    stop('Duplicate directed CEPII pairs; investigate before estimation')
  pair <- merge(pair,ce,by.x=c('ce_origin','ce_destination'),
                by.y=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  pair[, valid_distance := !is.na(distw) & is.finite(distw) & distw>0]
  # Complete EU27 CEPII coverage is a transparent H1 sample restriction,
  # NOT an assertion that excluded countries have zero trade.
  all_pairs <- data.table::CJ(importer=EU27,supplier=sort(unique(d$supplier)))
  all_pairs[, ce_origin := bridge(supplier)]
  all_pairs[, ce_destination := bridge(importer)]
  all_pairs <- merge(all_pairs,ce,by.x=c('ce_origin','ce_destination'),
                     by.y=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  all_pairs[, valid_distance := !is.na(distw) & is.finite(distw) & distw>0]
  complete <- all_pairs[,.(all_27=all(valid_distance)),by=supplier]
  excluded <- complete[all_27==FALSE,supplier]
  selected <- complete[all_27==TRUE,supplier]
  if (!length(selected)) stop('No suppliers with complete EU27 distances')
  d <- d[supplier %in% selected]
  pair <- pair[valid_distance==TRUE & supplier %in% selected,
               .(importer,supplier,distw)]
  d <- merge(d,pair,by=c('importer','supplier'),all.x=TRUE,sort=FALSE)
  if (anyNA(d$distw) || any(d$distw<=0)) stop('Unmatched distance in H1 sample')
  d[, logdist := log(distw)]
  d[, post := as.integer(year>=2020L)]
  d[, post_logdist := post*logdist]

  output_dir <- file.path(project_dir,'model_results','H2_distance')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  d[, year_num := year - 2015L]
  d[, trend_logdist := logdist * year_num]
  data.table::fwrite(d[,.(year,hs2,rows=.N,importers=data.table::uniqueN(importer),
                          suppliers=data.table::uniqueN(supplier)),by=.(year,hs2)],
                     file.path(output_dir,'H2_distance_coverage.csv'))
  data.table::fwrite(data.table::data.table(supplier=sort(selected)),
                     file.path(output_dir,'H2_distance_eligible_suppliers.csv'))
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  # Sector-specific interactions are all estimated in ONE model, not 68
  # disconnected fits. No global post*distance slope is imposed.
  pooled_fe <- ~ importer^supplier^hs2 + importer^hs2^year
  specifications <- list(
    list(name='baseline_2015_2025', data=d,
         formula=value ~ i(hs2,post_logdist) | importer^supplier^hs2 + importer^hs2^year),
    list(name='preH6_2015_2021', data=d[year<=2021L],
         formula=value ~ i(hs2,post_logdist) | importer^supplier^hs2 + importer^hs2^year),
    list(name='linear_distance_trend', data=d,
         formula=value ~ i(hs2,post_logdist) + trend_logdist | importer^supplier^hs2 + importer^hs2^year)
  )
  extract <- function(fit, spec) {
    ct <- fixest::coeftable(fit)
    terms <- grep('^hs2::.*:post_logdist$',rownames(ct),value=TRUE)
    if (!length(terms)) stop('Cannot find sector-distance terms in ',spec,
                             '. Observed terms: ',paste(head(rownames(ct),6),collapse='; '))
    sectors <- sub('^hs2::(.*):post_logdist$','\\1',terms)
    out <- data.table::data.table(specification=spec,hs2=sectors,
       term=terms,estimate=as.numeric(ct[terms,1]),se=as.numeric(ct[terms,2]),
       p_value=as.numeric(ct[terms,4]),n_estimated=as.integer(stats::nobs(fit)))
    out[,`:=`(p_BH=stats::p.adjust(p_value,'BH'),
              effect_pct=100*expm1(estimate),
              ci_low=estimate-1.96*se,ci_high=estimate+1.96*se)]
    # Direct test of equal sector-specific coefficients, not a joint zero test.
    v <- stats::vcov(fit)[terms,terms,drop=FALSE]
    k <- length(terms)
    C <- cbind(diag(k-1),rep(-1,k-1))
    delta <- as.vector(C %*% ct[terms,1])
    V <- C %*% v %*% t(C)
    eg <- eigen((V+t(V))/2,symmetric=TRUE)
    tol <- max(abs(eg$values))*1e-9
    keep <- eg$values>tol
    rank <- sum(keep)
    if (rank>0) {
      score <- crossprod(eg$vectors[,keep,drop=FALSE],delta)
      statistic <- sum(as.numeric(score)^2/eg$values[keep])
      p <- stats::pchisq(statistic,df=rank,lower.tail=FALSE)
    } else { statistic <- NA_real_; p <- NA_real_ }
    test <- data.table::data.table(specification=spec,sectors=k,
      contrast_count=k-1,covariance_rank=rank,chi_square=statistic,
      df=rank,p_value=p,rank_deficient=rank<k-1)
    list(sectors=out,heterogeneity=test)
  }
  results <- list(); tests <- list()
  for (x in specifications) {
    cat('H2 distance pooled PPML: ',x$name,'; input rows ',nrow(x$data),'\n',sep='')
    fit <- fixest::fepois(x$formula,data=x$data,vcov=~supplier,
                         notes=TRUE,warn=TRUE)
    obj <- extract(fit,x$name)
    results[[x$name]] <- obj$sectors
    tests[[x$name]] <- obj$heterogeneity
    data.table::fwrite(data.table::rbindlist(results),
                       file.path(output_dir,'H2_distance_sector_results_progress.csv'))
    data.table::fwrite(data.table::rbindlist(tests),
                       file.path(output_dir,'H2_distance_heterogeneity_tests_progress.csv'))
    rm(fit); invisible(gc(verbose=FALSE))
  }
  # Pre-period diagnostic: 2015-2019 sector-specific distance slopes.
  # This does NOT prove parallel trends, even if individually insignificant.
  pre <- d[year<=2019L]
  cat('H2 distance pre-period diagnostic: 2015-2019\n')
  prefit <- fixest::fepois(
    value ~ i(hs2,trend_logdist) | importer^supplier^hs2 + importer^hs2^year,
    data=pre,vcov=~supplier,notes=TRUE,warn=TRUE)
  pc <- fixest::coeftable(prefit)
  pt <- grep('^hs2::.*:trend_logdist$',rownames(pc),value=TRUE)
  preout <- data.table::data.table(hs2=sub('^hs2::(.*):trend_logdist$','\\1',pt),
     trend_estimate=as.numeric(pc[pt,1]),trend_se=as.numeric(pc[pt,2]),
     trend_p_value=as.numeric(pc[pt,4]))
  preout[,trend_p_BH:=stats::p.adjust(trend_p_value,'BH')]
  data.table::fwrite(preout,file.path(output_dir,'H2_distance_pretrend.csv'))
  rm(prefit); invisible(gc(verbose=FALSE))
  out <- data.table::rbindlist(results)
  out <- merge(out,preout,by='hs2',all.x=TRUE,sort=FALSE)
  data.table::setorder(out,specification,hs2)
  data.table::fwrite(out,file.path(output_dir,'H2_distance_sector_results.csv'))
  tests_out <- data.table::rbindlist(tests)
  data.table::fwrite(tests_out,file.path(output_dir,'H2_distance_heterogeneity_tests.csv'))
  writeLines(c('H2 SECTORAL DISTANCE AUDIT',paste('Run:',Sys.time()),
    paste('H1-compatible input rows:',nrow(d)),
    paste('H1-compatible suppliers:',length(selected)),
    paste('Eligible sectors:',data.table::uniqueN(d$hs2)),
    paste('Excluded CEPII-incomplete suppliers:',paste(excluded,collapse=', ')),
    'Pooled sector-specific post2020 x log CEPII distance coefficients.',
    'Importer-supplier-HS2 and importer-HS2-year fixed effects.',
    'Supplier-clustered SE; covariance rank checked for heterogeneity Wald tests.',
    'Sector BH adjustments applied separately within each specification.',
    'Linear-distance-trend sensitivity imposes one common distance trend.',
    '2015-2019 pretrend diagnostic estimates sector-specific linear slopes.',
    '2015-2021 sensitivity excludes H6, but H4/H5 remain unharmonized.',
    'Observed-only trade; missing cells NOT filled with zero.',
    'Associations, not causal effects; no randomization or exogenous shock identified.',
    'Pretrend failures and HS vintages must be discussed; no automatic H3 selection.'),
    file.path(output_dir,'H2_distance_audit.txt'))
  cat('H2 DISTANCE COMPLETED: ',output_dir,'\n',sep='')
  print(tests_out)
  print(out[,.(positive=sum(estimate>0,na.rm=TRUE),
       negative=sum(estimate<0,na.rm=TRUE),
       negative_BH05=sum(estimate<0 & p_BH<.05,na.rm=TRUE)),by=specification])
}

# FINAL H2 VALIDATION (one additional prespecified specification)
if (identical(ibr_model_mode,'h2_validation')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir,'model_results','H1')
  for (p in c('data.table','fixest','readxl','countrycode')) {
    if (!requireNamespace(p,quietly=TRUE))
      stop('Install required package: ',p)
  }
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  distance_path <- file.path(project_dir,'dist_cepii.xlsx')
  if (!file.exists(input_path) || !file.exists(distance_path))
    stop('Missing integrated trade archive or dist_cepii.xlsx')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
            'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
            'POL','PRT','ROU','SVK','SVN','ESP','SWE')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  raw <- data.table::as.data.table(readRDS(input_path))
  if (nrow(raw)!=1499841L) stop('Unexpected integrated record count')
  fields <- c('reporter_iso','partner_iso','ref_year','cmd_code',
              'flow_code','aggr_level','primary_value','classification_code')
  if (!all(fields %in% names(raw))) stop('Missing trade columns')
  d <- raw[,.(importer=toupper(trimws(as.character(reporter_iso))),
             supplier=toupper(trimws(as.character(partner_iso))),
             year=as.integer(as.character(ref_year)),
             hs2=sprintf('%02d',as.integer(as.character(cmd_code))),
             flow=as.character(flow_code),
             aggr=as.integer(as.character(aggr_level)),
             value=as.numeric(as.character(primary_value)),
             vintage=as.character(classification_code))]
  if (anyNA(d[,.(importer,supplier,year,hs2,flow,aggr,value,vintage)]))
    stop('Missing trade fields')
  if (any(!is.finite(d$value)) || any(d$value<0)) stop('Invalid trade values')
  d <- d[importer %in% EU27 & year %in% 2015:2025 &
         hs2 %in% HS68 & flow=='M' & aggr==2L]
  valid_iso <- unique(c(stats::na.omit(countrycode::codelist$iso3c),'XKX'))
  d <- d[supplier %in% valid_iso & !supplier %in% EU27 & supplier!=importer]
  if (anyDuplicated(d,by=c('importer','supplier','year','hs2')))
    stop('Duplicate model keys')
  vintage_expected <- c('H4','H4','H5','H5','H5','H5','H5','H6','H6','H6','H6')
  if (any(d$vintage != vintage_expected[d$year-2014L]))
    stop('Unexpected HS vintage; review data')

  ce <- data.table::as.data.table(readxl::read_excel(distance_path))
  if (!all(c('iso_o','iso_d','distw') %in% names(ce)))
    stop('CEPII must contain iso_o, iso_d, distw')
  ce <- ce[,.(iso_o=toupper(trimws(as.character(iso_o))),
             iso_d=toupper(trimws(as.character(iso_d))),
             distw=as.numeric(distw))]
  # Direction: origin is supplier, destination is EU importer.
  # Historical code bridges are used ONLY for the CEPII lookup.
  bridge <- function(x) {
    x[x=='ROU'] <- 'ROM'
    x[x=='SRB'] <- 'YUG'
    x
  }
  d[, ce_origin := bridge(supplier)]
  d[, ce_destination := bridge(importer)]
  pair <- unique(d[,.(supplier,importer,ce_origin,ce_destination)])
  ce <- ce[!is.na(iso_o) & !is.na(iso_d)]
  if (anyDuplicated(ce,by=c('iso_o','iso_d')))
    stop('Duplicate directed CEPII pairs; investigate before estimation')
  pair <- merge(pair,ce,by.x=c('ce_origin','ce_destination'),
                by.y=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  pair[, valid_distance := !is.na(distw) & is.finite(distw) & distw>0]
  # Complete EU27 CEPII coverage is a transparent H1 sample restriction,
  # NOT an assertion that excluded countries have zero trade.
  all_pairs <- data.table::CJ(importer=EU27,supplier=sort(unique(d$supplier)))
  all_pairs[, ce_origin := bridge(supplier)]
  all_pairs[, ce_destination := bridge(importer)]
  all_pairs <- merge(all_pairs,ce,by.x=c('ce_origin','ce_destination'),
                     by.y=c('iso_o','iso_d'),all.x=TRUE,sort=FALSE)
  all_pairs[, valid_distance := !is.na(distw) & is.finite(distw) & distw>0]
  complete <- all_pairs[,.(all_27=all(valid_distance)),by=supplier]
  excluded <- complete[all_27==FALSE,supplier]
  selected <- complete[all_27==TRUE,supplier]
  if (!length(selected)) stop('No suppliers with complete EU27 distances')
  d <- d[supplier %in% selected]
  pair <- pair[valid_distance==TRUE & supplier %in% selected,
               .(importer,supplier,distw)]
  d <- merge(d,pair,by=c('importer','supplier'),all.x=TRUE,sort=FALSE)
  if (anyNA(d$distw) || any(d$distw<=0)) stop('Unmatched distance in H1 sample')
  d[, logdist := log(distw)]
  d[, post := as.integer(year>=2020L)]
  d[, post_logdist := post*logdist]

  output_dir <- file.path(project_dir,'model_results','H2_distance','validation')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  d[, year_num := year - 2015L]
  d[, trend_logdist := logdist * year_num]
  data.table::fwrite(d[,.(year,hs2,rows=.N,importers=data.table::uniqueN(importer),
                          suppliers=data.table::uniqueN(supplier)),by=.(year,hs2)],
                     file.path(output_dir,'validation_coverage.csv'))
  data.table::fwrite(data.table::data.table(supplier=sort(selected)),
                     file.path(output_dir,'validation_suppliers.csv'))
  fixest::setFixest_nthreads(max(1L,parallel::detectCores(logical=FALSE)-1L))
  # Sector-specific interactions are all estimated in ONE model, not 68
  # disconnected fits. No global post*distance slope is imposed.
  pooled_fe <- ~ importer^supplier^hs2 + importer^hs2^year

  # Confirmatory diagnostic: allow EACH sector its own linear distance trend.
  cat('Validation: sector-specific distance trends (one pooled PPML)\n')
  fit <- fixest::fepois(
    value ~ i(hs2,post_logdist) + i(hs2,trend_logdist) |
      importer^supplier^hs2 + importer^hs2^year,
    data=d,vcov=~supplier,notes=TRUE,warn=TRUE)
  ct <- fixest::coeftable(fit)
  terms <- grep('^hs2::.*:post_logdist$',rownames(ct),value=TRUE)
  if(length(terms)!=68L) stop('Expected 68 post terms; got ',length(terms))
  out <- data.table::data.table(
    hs2=sub('^hs2::(.*):post_logdist$','\\1',terms),
    estimate=as.numeric(ct[terms,1]),se=as.numeric(ct[terms,2]),
    p_value=as.numeric(ct[terms,4]))
  out[,`:=`(p_BH=stats::p.adjust(p_value,'BH'),
            effect_pct=100*expm1(estimate))]
  data.table::fwrite(out,file.path(output_dir,'H2_validation_sector_trends.csv'))
  # Numerical audit of the joint Wald statistic and supplier clustering.
  v <- stats::vcov(fit)[terms,terms,drop=FALSE]
  k <- length(terms)
  C <- cbind(diag(k-1),rep(-1,k-1))
  V <- C %*% v %*% t(C)
  ev <- eigen((V+t(V))/2,symmetric=TRUE,only.values=TRUE)$values
  tol <- max(abs(ev))*1e-9
  diagnostics <- data.table::data.table(
    n_obs=as.integer(stats::nobs(fit)),n_suppliers=data.table::uniqueN(d$supplier),
    n_terms=k,contrast_rank=sum(ev>tol),
    min_eigen=min(ev),max_eigen=max(ev),
    condition_number=if(min(ev)>0) max(ev)/min(ev) else Inf,
    negative_eigenvalues=sum(ev< -tol),
    missing_estimates=sum(!is.finite(out$estimate)),
    missing_pvalues=sum(!is.finite(out$p_value)))
  data.table::fwrite(diagnostics,file.path(output_dir,'H2_validation_covariance_audit.csv'))
  # Compare previously exported coefficients without rerunning the old models.
  previous <- file.path(project_dir,'model_results','H2_distance',
                        'H2_distance_sector_results.csv')
  if(file.exists(previous)) {
    old <- data.table::fread(previous)
    old <- old[specification=='baseline_2015_2025',.(hs2=as.character(hs2),
                  baseline_estimate=estimate,baseline_p_BH=p_BH)]
    comp <- merge(out,old,by='hs2',all.x=TRUE)
    comp[,`:=`(baseline_negative_BH=baseline_estimate<0 & baseline_p_BH<.05,
              validation_negative_BH=estimate<0 & p_BH<.05)]
    data.table::fwrite(comp,file.path(output_dir,'H2_validation_comparison.csv'))
  }
  writeLines(c('H2 VALIDATION AUDIT',
     'One additional pooled PPML: 68 sector-specific post-distance effects AND 68 sector-specific linear distance trends.',
     'No new data downloads or new supplier selection.',
     'Supplier-clustered inference; 182 supplier clusters.','Joint Wald test not relied upon without numerical review.',
     'HS vintages not harmonized; observed-only trade; no causal identification.',
     paste('Timestamp:',Sys.time())),file.path(output_dir,'H2_validation_audit.txt'))
  cat('H2 VALIDATION COMPLETED: ',output_dir,'\n',sep='')
  print(out[hs2 %in% c('36','50','88')])
  print(diagnostics)
}


# =============================================================================
# H3 EXPLORATORY TURKIYE FOLLOW-UP: HS36 AND HS88 ONLY
# Sectors were selected using H2 results; NOT an independent confirmatory test.
# Run: options(ibr.model.mode='h3_turkey'); source('.../02_runmodels.R')
# No API calls or additional downloads. Retains the frozen 31-country control set.
# =============================================================================
if (identical(ibr_model_mode,'h3_turkey')) {
  project_dir <- ibr_project_dir("02_runmodels.R")
  output_dir <- file.path(project_dir,'model_results','H3_Turkiye')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  for (pkg in c('data.table','fixest')) if (!requireNamespace(pkg,quietly=TRUE)) stop('Missing package: ',pkg)
  input_path <- file.path(project_dir,'trade_data_integrated.rds')
  controls_path <- file.path(project_dir,'control_candidates_provisional.csv')
  if (!file.exists(input_path) || !file.exists(controls_path)) stop('Missing existing input data')
  ctrl <- data.table::fread(controls_path)
  controls <- sort(unique(ctrl[provisional_control==TRUE & eligible_EU27==TRUE & farther_than_TUR==TRUE,as.character(partner_iso)]))
  if (length(controls)!=31L || 'TUR' %in% controls) stop('Frozen control set invalid')
  EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA','DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','ESP','SWE')
  sectors <- c('36','88')
  raw <- data.table::as.data.table(readRDS(input_path))
  if (nrow(raw)!=1499841L) stop('Unexpected integrated data size')
  d <- raw[,.(importer=toupper(trimws(as.character(reporter_iso))),supplier=toupper(trimws(as.character(partner_iso))),year=as.integer(as.character(ref_year)),hs2=sprintf('%02d',as.integer(as.character(cmd_code))),flow=as.character(flow_code),aggr=as.integer(as.character(aggr_level)),vintage=as.character(classification_code),value=as.numeric(as.character(primary_value)))]
  d <- d[importer %in% EU27 & supplier %in% c('TUR',controls) & year %in% 2015:2025 & hs2 %in% sectors & flow=='M' & aggr==2L]
  if (!nrow(d) || anyNA(d[,.(importer,supplier,year,hs2,value,vintage)]) || any(!is.finite(d$value)) || any(d$value<0)) stop('Invalid data')
  if (anyDuplicated(d,by=c('importer','supplier','year','hs2'))) stop('Duplicate trade cells')
  vintage_map <- c('2015'='H4','2016'='H4','2017'='H5','2018'='H5','2019'='H5','2020'='H5','2021'='H5','2022'='H6','2023'='H6','2024'='H6','2025'='H6')
  if (any(d$vintage != unname(vintage_map[as.character(d$year)]))) stop('Unexpected HS classification')
  if (!setequal(unique(d$supplier),c('TUR',controls)) || !setequal(unique(d$importer),EU27)) stop('Missing importer or supplier')
  d[,`:=`(tur=as.integer(supplier=='TUR'),post=as.integer(year>=2020L),year_num=year-2015L)]
  d[,did:=tur*post]
  data.table::fwrite(d[,.(observations=.N,suppliers=data.table::uniqueN(supplier),importers=data.table::uniqueN(importer),tur_observations=sum(tur==1L),trade_value=sum(value)),by=.(hs2,year,tur)],file.path(output_dir,'H3_coverage.csv'))
  # Descriptive observed trade totals are NOT complete market shares: missing cells are not zero-filled.
  descriptive <- d[,.(observed_value=sum(value),observed_cells=.N),by=.(hs2,year,tur)]
  data.table::fwrite(descriptive,file.path(output_dir,'H3_observed_trade_totals.csv'))
  get_term <- function(m,pattern) {
    ct <- fixest::coeftable(m)
    keys <- grep(pattern,rownames(ct),value=TRUE)
    if (length(keys)!=1L) stop('Unexpected coefficient names: ',paste(rownames(ct),collapse=', '))
    as.numeric(ct[keys,c(1,2,4)])
  }
  rows <- list(); pre_rows <- list(); k <- 0L
  for (sector in sectors) {
    z <- d[hs2==sector]
    for (spec in c('baseline_2015_2025','preH6_2015_2021','turkey_linear_trend')) {
      zz <- if (spec=='preH6_2015_2021') z[year<=2021L] else z
      fml <- if (spec=='turkey_linear_trend') value ~ did + I(tur*year_num) | importer^supplier + importer^year else value ~ did | importer^supplier + importer^year
      cat('H3:',sector,spec,'rows',nrow(zz),'\n')
      fit <- fixest::fepois(fml,data=zz,vcov=~supplier,notes=TRUE,warn=TRUE)
      b <- get_term(fit,'^did$')
      k <- k+1L
      rows[[k]] <- data.table::data.table(hs2=sector,specification=spec,estimate=b[1],se=b[2],p_value=b[3],effect_pct=100*expm1(b[1]),ci_low_pct=100*expm1(b[1]-1.96*b[2]),ci_high_pct=100*expm1(b[1]+1.96*b[2]),n_input=nrow(zz),n_estimated=as.integer(stats::nobs(fit)),suppliers=data.table::uniqueN(zz$supplier),tur_pre=sum(zz$tur==1L & zz$post==0L),tur_post=sum(zz$tur==1L & zz$post==1L))
      rm(fit)
    }
    pre <- z[year<=2019L]
    prefit <- fixest::fepois(value ~ I(tur*year_num) | importer^supplier + importer^year,data=pre,vcov=~supplier,notes=TRUE,warn=TRUE)
    b <- get_term(prefit,'tur.*year_num|year_num.*tur')
    pre_rows[[length(pre_rows)+1L]] <- data.table::data.table(hs2=sector,pretrend_estimate=b[1],pretrend_se=b[2],pretrend_p=b[3],n_estimated=as.integer(stats::nobs(prefit)))
    rm(prefit)
  }
  out <- data.table::rbindlist(rows)
  # Two sectors are H2-selected; p-values are descriptive, not independent confirmation.
  data.table::fwrite(out,file.path(output_dir,'H3_Turkiye_sector_results.csv'))
  data.table::fwrite(data.table::rbindlist(pre_rows),file.path(output_dir,'H3_Turkiye_pretrends.csv'))
  writeLines(c('H3 EXPLORATORY TURKIYE FOLLOW-UP',paste('Run:',Sys.time()),'HS36 and HS88 were selected based on H2 distance results; selection is data-dependent.','Frozen pre-2020 31 farther non-EU suppliers; no country reselection.','Three specifications: full baseline, pre-H6 2015-2021, TUR-specific linear time trend.','Supplier-clustered SE; ONLY ONE treated supplier (TUR). Standard p-values are screening/descriptive, not reliable confirmatory causal inference.','Pre-period TUR-specific trend diagnostic (2015-2019).','Observed trade only: missing cells are not imputed as zeros.','HS2 vintages H4/H5/H6 not harmonized; preH6 still includes H4/H5.','The comparison is conditional relative trade performance, NOT a direct estimate of nearshoring effects.','No new downloads or new models beyond these prespecified checks.'),file.path(output_dir,'H3_Turkiye_audit.txt'))
  cat('H3 TURKIYE COMPLETED:',output_dir,'\n')
  print(out[,.(hs2,specification,estimate,se,p_value,effect_pct)])
  print(data.table::rbindlist(pre_rows))
}

# FINAL_ALL - Final manuscript models, recomputed from local data
if (identical(ibr_model_mode, "final_all")) {

  ibr_run_final_all <- function() {

    project_dir <- ibr_project_dir("02_runmodels.R")

    required <- c(
      "trade_data_integrated.rds",
      "dist_cepii.xlsx",
      "control_candidates_provisional.csv"
    )

    missing <- required[
      !file.exists(file.path(project_dir, required))
    ]

    if (length(missing)) {
      stop("Missing inputs: ", paste(missing, collapse = ", "))
    }

    packages <- c("data.table", "fixest", "readxl", "countrycode")
    absent <- packages[
      !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
    ]

    if (length(absent)) {
      stop("Missing packages: ", paste(absent, collapse = ", "))
    }

    result_root <- file.path(project_dir, "model_results")

    if (file.exists(result_root)) {
      stop("model_results already exists. Rename or back it up first.")
    }

    script <- file.path(project_dir, "02_runmodels.R")

    steps <- list(
      h1 = "H1/H1_main_PPML.csv",
      h1_checks = "H1/checks/H1_sensitivity_results.csv",
      h1_final_checks = "H1/final_checks/H1_final_robustness_results.csv",
      h2_distance = "H2_distance/H2_distance_sector_results.csv",
      h2_validation = c(
        "H2_distance/validation/H2_validation_sector_trends.csv",
        "H2_distance/validation/H2_validation_comparison.csv"
      ),
      h3_turkey = c(
        "H3_Turkiye/H3_Turkiye_sector_results.csv",
        "H3_Turkiye/H3_Turkiye_pretrends.csv"
      )
    )

    original_mode <- getOption("ibr.model.mode")
    on.exit(options(ibr.model.mode = original_mode), add = TRUE)

    started <- Sys.time()
    completed <- character()

    for (step in names(steps)) {

      cat("\nFINAL_ALL: starting ", step, "\n", sep = "")

      options(ibr.model.mode = step)

      sys.source(
        script,
        envir = new.env(parent = globalenv()),
        keep.source = FALSE
      )

      expected <- file.path(result_root, steps[[step]])
      missing_outputs <- expected[!file.exists(expected)]

      if (length(missing_outputs)) {
        stop(
          "Step ", step, " did not produce: ",
          paste(missing_outputs, collapse = ", ")
        )
      }

      completed <- c(completed, step)
      cat("FINAL_ALL: completed ", step, "\n", sep = "")
    }

    writeLines(
      c(
        "FINAL_ALL SUCCESS",
        paste("Started:", started),
        paste("Finished:", Sys.time()),
        paste("Completed:", paste(completed, collapse = ", ")),
        "Inputs: local integrated trade data and local auxiliary files",
        "No API downloads; no missing-trade zero imputation",
        "HS vintages not harmonized; no causal identification",
        "Turkey results exploratory; one treated supplier"
      ),
      file.path(result_root, "FINAL_ALL_run_audit.txt")
    )

    cat(
      "\nFINAL_ALL SUCCESS: all final-model output files verified.\n"
    )

    invisible(completed)
  }

  ibr_run_final_all()
}
