# =============================================================================
# 01_data.R | FINAL OFFLINE DATA INTEGRATION + NONEXECUTABLE PROVENANCE HISTORY
# =============================================================================
# ACTIVE workflow: source("01_data.R", encoding="UTF-8")
# All 1,269 raw .rds files must be directly inside ./rawdata (no subfolders).
# This script NEVER calls the API or downloads data.
# Historical acquisition and screening code is preserved verbatim below
# inside if (FALSE) and CANNOT execute during source().
# Historical absolute paths and old folder names below are documentary only.
# The active integration code uses only the folder containing this script.
#
# Source groups identified by filename:
#   original: 297   *_HS2.rds or *_HS2_selected.rds
#   control:  162   *_HS2_extra.rds
#   H1:       810   *_HS2_H1_batchNN.rds
# =============================================================================

# HISTORICAL CODE BELOW IS PERMANENTLY NONEXECUTABLE
if (FALSE) {
# =============================================================================
# SINGLE ENTRY POINT - 01_data.R
# Existing data build/control screening preserved. H1 stage is opt-in.
# Mode: 'original' (default), 'preview', 'pilot', 'download', 'integrate'.
# Do NOT rename or move rawdata or rawdata_control_extension.
# =============================================================================
ibr_h1_mode <- getOption('ibr.h1.mode', 'original')
if (!ibr_h1_mode %in% c('original','preview','pilot','download','integrate'))
  stop('ibr.h1.mode must be original, preview, pilot, download, or integrate')
if (identical(ibr_h1_mode, 'original')) {
# =============================================================================
# 01_data.R | ARSIVIN OLUSTURULMASINDA KULLANILAN API SORGULARININ BELGESI
# BU BOLUMUN TAMAMI YORUMDUR (#); source() ILE CALISMAZ.
# =============================================================================
# Kaynak: UN Comtrade; comtradr 1.0.6, comtradr::ct_get_data().
# Asagidaki iki cagrinin parametreleri onceki indirme kodlarinin
# belgelenmis ayarlarindan YENIDEN OLUSTURULMUSTUR. Bunlar orijinal
# betiklerin birebir kopyasi veya yeniden calistirilmis sorgular DEGILDIR.
# Orijinal kod adlari: 00_screening_v5_1.R ve 04_postpandemic_download.R.
# Eski is akisi ayri R surecinde callr::r() kullanmis; 150 sn zaman asimi,
# istekler arasinda 3 sn bekleme ve mevcut dosyayi koruma uygulamistir.
# API anahtari eskiden COMTRADE_PRIMARY degiskeninden okunuyordu;
# anahtarin kendisi bu dosyaya kaydedilmez. Primary anahtarinin daha sonra
# HTTP 401 verdigi, Secondary anahtarinin bazi sorgularda calistigi goruldu.
# Buradaki ornekler yalnizca PROVENANS BELGESIDIR; API'ye baglanmaz.
#
# ORTAK PARAMETRELER:
# hs2_codes <- sprintf('%02d', c(28:76, 78:96))
#
# 2015-2019: 27 AB raporlayici x 5 yil = 135 dosya.
# Her raporlayici/yil icin tarihsel sorgu bicimi:
# pre_result <- comtradr::ct_get_data(
#   reporter = reporter_iso,
#   partner = 'all_countries',
#   flow_direction = 'Import',
#   commodity_classification = 'HS',
#   commodity_code = hs2_codes,
#   start_date = yr,
#   end_date = yr,
#   frequency = 'A',
#   verbose = FALSE,
#   cache = FALSE
# )
# # Arsiv dosyasi ornegi: rawdata/AUT_2015_HS2.rds
#
# 2020-2025: 27 AB raporlayici x 6 yil = 162 dosya.
# Her raporlayici/yil icin tarihsel sorgu bicimi:
# post_result <- comtradr::ct_get_data(
#   reporter = reporter_iso,
#   partner = c('TUR', 'IND', 'KOR', 'JPN', 'THA'),
#   flow_direction = 'Import',
#   commodity_classification = 'HS',
#   commodity_code = hs2_codes,
#   start_date = yr,
#   end_date = yr,
#   frequency = 'A',
#   verbose = FALSE,
#   cache = FALSE
# )
# # Arsiv dosyasi ornegi: rawdata/AUT_2020_HS2_selected.rds
#
# NOTLAR:
# - 2015-2019 indirme denetimi: 130 OK, 5 INCOMPLETE (ROU HS71/HS93).
# - 2020-2025 indirme denetimi: 162 SAVED; tam kapsama kaniti degil.
# - H4 (2015-16), H5 (2017-21), H6 (2022-25) HS versiyonlari farklidir.
# - Comtrade Plus, ROU/2015-2019/HS71,HS93/World/Imports sorgusunda
#   2026-10-26 tarihinde 0 sonuc gostermistir; nedeni bilinmemektedir.
# - Eski indirme islemlerini birebir yeniden uretmek icin orijinal iki
#   betik, API yanitlari ve indirme gunlukleri ayrica saklanmalidir.
# - ASAGIDAKI AKTIF KOD YALNIZCA rawdata/ ARSIVINI OKUR, API CAGIRMAZ.
# =============================================================================

# =============================================================================
# 01_data.R | REPRODUCIBLE DATASET BUILD FROM LOCAL UN COMTRADE ARCHIVE
# =============================================================================
# INPUT:  rawdata/ (297 original RDS files)
# OUTPUT: trade_data.rds, data_audit.txt, missing_hs2_coverage.csv
# NO API requests; NO changes to rawdata; NO imputation of missing trade.
# The archived data were downloaded previously, not by this script.
# Pre-period: all available partners (2015-2019).
# Post-period: only TUR, IND, KOR, JPN, THA (2020-2025).
# HS vintages H4/H5/H6 differ across years; this is not a harmonized panel.
# Romania HS71/HS93 in 2015-2019: unavailable in original archive and
# Comtrade Plus World-import preview (checked 2026-10-26); cause unknown.
# =============================================================================

# 01_data.R ??? Rethinking Nearshoring: data preparation and audit
# Source of truth: Article_IBR_FINAL/rawdata/*.rds (297 archived Comtrade files).
# No API calls. The 297 archived RDS files are read-only; no previous model results are read.
# Outputs: trade_data.rds and data_audit.txt in Article_IBR_FINAL.
# This script constructs the archived dataset and audits pre-2020 coverage.
# Run in RStudio: source('C:/Users/rstat/Documents/Article_IBR_FINAL/01_data.R', encoding='UTF-8')

project_dir <- 'C:/Users/rstat/Documents/Article_IBR_FINAL'
raw_dir <- file.path(project_dir, 'rawdata')
stopifnot(dir.exists(project_dir), dir.exists(raw_dir))

# SOURCE-VERIFIED DATA LIMITATION (26 October 2026):
# UN Comtrade Plus preview for Romania / imports / World / HS71 and HS93 /
# 2015-2019 returned zero results. This establishes unavailability under those
# query settings, NOT the reporting country's motive or absence of all partner data.
# No API calls, imputations, or changes to archived raw RDS files are permitted.

files <- sort(list.files(raw_dir, pattern='\\.rds$', full.names=TRUE, ignore.case=TRUE))
if (length(files) != 297L) stop('Expected 297 .rds files in rawdata; found ', length(files), '. Nothing written.')
if (anyDuplicated(basename(files))) stop('Duplicate raw file names detected. Nothing written.')
if (sum(grepl('_selected\\.rds$', basename(files))) != 162L) stop('Expected 162 selected files. Nothing written.')

# Expect exactly 27 reporting countries x 11 years; filename format varies by period.
meta <- regexec('^([A-Z]{3})_(20[0-9]{2})_HS2(_selected)?\\.rds$', basename(files))
parts <- regmatches(basename(files), meta)
if (any(lengths(parts) == 0L)) stop('Unexpected file name(s): ', paste(basename(files)[lengths(parts)==0L], collapse=', '))
reporter_from_name <- vapply(parts, `[`, character(1), 2L)
year_from_name <- as.integer(vapply(parts, `[`, character(1), 3L))
file_grid <- data.frame(reporter=reporter_from_name, year=year_from_name, file=basename(files), stringsAsFactors=FALSE)
if (anyDuplicated(file_grid[c('reporter','year')])) stop('Duplicate reporter-year files. Nothing written.')
if (length(unique(file_grid$reporter)) != 27L || !identical(sort(unique(file_grid$year)), 2015:2025)) {
  stop('Reporter/year coverage differs from expected 27 reporters and 2015???2025. Nothing written.')
}
if (nrow(file_grid) != length(unique(file_grid$reporter)) * 11L) stop('Missing reporter-year files. Nothing written.')
if (any(grepl('_selected\\.rds$', file_grid$file) != (file_grid$year >= 2020L))) stop('Filename suffix does not match year period.')

required <- c('reporter_iso','partner_iso','ref_year','cmd_code','aggr_level',
              'classification_code','flow_code','primary_value','qty','qty_unit_abbr',
              'net_wgt','is_aggregate','is_reported')
raw_list <- vector('list', length(files))
checks <- vector('list', length(files))
for (k in seq_along(files)) {
  x <- tryCatch(readRDS(files[k]), error=function(e) stop('Cannot read ', basename(files[k]), ': ', conditionMessage(e)))
  if (!is.data.frame(x)) stop('Not a data.frame: ', basename(files[k]))
  absent <- setdiff(required, names(x))
  if (length(absent)) stop('Missing fields in ', basename(files[k]), ': ', paste(absent, collapse=', '))
  if (nrow(x) && (any(is.na(x$reporter_iso)) || any(as.character(x$reporter_iso) != reporter_from_name[k]))) {
    stop('Reporter code inconsistent with file name: ', basename(files[k]))
  }
  if (nrow(x) && (any(is.na(x$ref_year)) || any(as.integer(as.character(x$ref_year)) != year_from_name[k]))) {
    stop('Year inconsistent with file name: ', basename(files[k]))
  }
  # The archived downloads should be annual HS2 import records.
  if (nrow(x) && any(as.character(x$flow_code) != 'M', na.rm=TRUE)) stop('Unexpected trade flow: ', basename(files[k]))
  if (nrow(x) && any(as.integer(as.character(x$aggr_level)) != 2L, na.rm=TRUE)) stop('Unexpected aggregation level: ', basename(files[k]))
  if (year_from_name[k] >= 2020L && nrow(x) &&
      any(!toupper(trimws(as.character(x$partner_iso))) %in% c('TUR','IND','KOR','JPN','THA'))) {
    stop('Unexpected supplier in selected-period file: ', basename(files[k]))
  }
  # Retain ALL original columns and a traceable source filename; do not aggregate.
  x$source_file <- basename(files[k])
  raw_list[[k]] <- x
  checks[[k]] <- data.frame(file=basename(files[k]), reporter=reporter_from_name[k],
                            year=year_from_name[k], rows=nrow(x),
                            classification=paste(sort(unique(as.character(x$classification_code))), collapse=';'),
                            distinct_partners=length(unique(as.character(x$partner_iso))),
                            stringsAsFactors=FALSE)
}

# Harmonize column types conservatively for row binding; original numeric fields stay numeric.
all_names <- unique(unlist(lapply(raw_list, names), use.names=FALSE))
for (k in seq_along(raw_list)) {
  missing_names <- setdiff(all_names, names(raw_list[[k]]))
  for (nm in missing_names) raw_list[[k]][[nm]] <- NA
  raw_list[[k]] <- raw_list[[k]][all_names]
}
# Base R rbind handles consistent Comtrade response schemas; fail loudly if incompatible.
trade_data <- tryCatch(do.call(rbind, raw_list), error=function(e)
  stop('Unable to combine raw records; inspect column types: ', conditionMessage(e)))
rownames(trade_data) <- NULL
file_checks <- do.call(rbind, checks)

# Do not treat missing trade cells as zero; do not treat physical quantities as comparable
# across products or units. Do not assume HS2012/2017/2022 classifications are harmonized.
# Keep aggregate rows in this audit dataset; model script must explicitly define eligible rows.
raw_hashes <- tools::md5sum(files)

lines <- c(
  'RETHINKING NEARSHORING ??? RAW DATA AUDIT',
  paste('Created:', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')),
  paste('Input folder:', raw_dir),
  paste('Raw files:', length(files)),
  paste('Combined rows:', nrow(trade_data)),
  paste('Reporting countries:', length(unique(trade_data$reporter_iso))),
  paste('Years:', paste(sort(unique(trade_data$ref_year)), collapse=', ')),
  paste('Supplier codes:', length(unique(trade_data$partner_iso))),
  '', 'FILE ROW COUNTS:',
  capture.output(print(file_checks, row.names=FALSE)),
  '', 'CLASSIFICATION BY YEAR (RECORD COUNTS):',
  capture.output(print(table(trade_data$ref_year, trade_data$classification_code, useNA='ifany'))),
  '', 'FLOW CODE COUNTS:',
  capture.output(print(table(trade_data$flow_code, useNA='ifany'))),
  '', 'AGGREGATION LEVEL COUNTS:',
  capture.output(print(table(trade_data$aggr_level, useNA='ifany'))),
  '', 'MISSINGNESS IN KEY FIELDS:',
  capture.output(print(sapply(trade_data[c('primary_value','qty','qty_unit_abbr','net_wgt','partner_iso','cmd_code')],
                              function(v) sum(is.na(v))))),
  '', 'SOURCE FILE MD5 HASHES:',
  paste(basename(names(raw_hashes)), unname(raw_hashes), sep='  '),
  '', 'IMPORTANT: This is a raw-record archive, not an estimation sample.',
  'No country, supplier, HS chapter, flow, year or record-status filters have been applied.',
  'H4/H5/H6 denote different HS classification vintages; comparability requires review.',
  '2015-2019 contain all_countries partners; 2020-2025 contain TUR/IND/KOR/JPN/THA only.',
  '2015-2019 original audit: 5 ROU files missing HS71 and HS93.',
  'Comtrade Plus preview (2026-10-26): Romania, HS71/93, 2015-2019, World imports: 0 results.',
  'The reason for source unavailability is unknown; no confidentiality claim is made.',
  'No missing partner/product/year combinations have been imputed as zero.',
  'No previous estimates or datasets were loaded; no API requests were made.'
)

# Write only after ALL validations and data assembly succeed.
saveRDS(trade_data, file.path(project_dir, 'trade_data.rds'), compress='xz')
writeLines(enc2utf8(lines), file.path(project_dir, 'data_audit.txt'), useBytes=TRUE)
cat('\nSUCCESS: trade_data.rds and data_audit.txt written to ', project_dir, '\n', sep='')
cat('Rows:', nrow(trade_data), '| Raw files:', length(files), '\n')
cat('NEXT: coverage diagnostics and data verification.\n')

# =============================================================================
# COVERAGE AUDIT: observed reporter-year-HS2 combinations, 2015-2019
# =============================================================================
EU27 <- c('AUT','BEL','BGR','HRV','CYP','CZE','DNK','EST','FIN','FRA',
          'DEU','GRC','HUN','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
          'POL','PRT','ROU','SVK','SVN','ESP','SWE')
HS68 <- sprintf('%02d', c(28:76, 78:96))
pre <- trade_data[as.integer(as.character(trade_data$ref_year)) %in% 2015:2019, , drop=FALSE]
pre$hs2_check <- sprintf('%02d', as.integer(as.character(pre$cmd_code)))
pre <- pre[pre$reporter_iso %in% EU27 & pre$hs2_check %in% HS68 &
             as.character(pre$flow_code)=='M' &
             as.integer(as.character(pre$aggr_level))==2L, , drop=FALSE]
expected <- expand.grid(reporter_iso=EU27, ref_year=2015:2019,
                        hs2_check=HS68, stringsAsFactors=FALSE)
observed <- unique(pre[c('reporter_iso','ref_year','hs2_check')])
expected$key <- paste(expected$reporter_iso, expected$ref_year, expected$hs2_check, sep='|')
observed$key <- paste(observed$reporter_iso, observed$ref_year, observed$hs2_check, sep='|')
missing <- expected[!expected$key %in% observed$key,
                    c('reporter_iso','ref_year','hs2_check'), drop=FALSE]
rownames(missing) <- NULL
utils::write.csv(missing, file.path(project_dir,'missing_hs2_coverage.csv'), row.names=FALSE)
cat('Missing pre-period reporter-year-HS2 combinations:', nrow(missing), '\n')
if (nrow(missing)) {
  print(missing, row.names=FALSE)
  cat('Coverage is incomplete. Missing observations have NOT been set to zero.\n')
  cat('Control-country selection is NOT finalized.\n')
}
write(c('', 'PRE-PERIOD MISSING REPORTER-YEAR-HS2 COMBINATIONS:',
        capture.output(print(missing, row.names=FALSE)),
        'No zero imputation. Control-country selection not finalized.'),
      file=file.path(project_dir,'data_audit.txt'), append=TRUE)
cat('DONE: trade_data.rds, data_audit.txt, missing_hs2_coverage.csv\n')

# =============================================================================
# EX-ANTE CONTROL SCREENING (2015-2019 ONLY; NO POST-TREATMENT INFORMATION)
# This stage documents ALL candidates. It does not cherry-pick based on effects.
# Coverage is observed presence, NOT proof that missing trade is zero.
# =============================================================================
if (!requireNamespace('countrycode', quietly=TRUE)) {
  stop('Control screening needs package countrycode. Install with install.packages("countrycode"). Raw outputs are unchanged.')
}
if (!requireNamespace('readxl', quietly=TRUE)) {
  stop('Control screening needs package readxl. Install with install.packages("readxl"). Raw outputs are unchanged.')
}

norm_iso <- function(z) toupper(trimws(as.character(z)))
screen <- pre
screen$reporter_iso <- norm_iso(screen$reporter_iso)
screen$partner_iso <- norm_iso(screen$partner_iso)
screen$ref_year <- as.integer(as.character(screen$ref_year))
screen$hs2 <- screen$hs2_check
screen$value <- suppressWarnings(as.numeric(as.character(screen$primary_value)))
if (anyNA(screen$value) || any(!is.finite(screen$value)) || any(screen$value < 0))
  stop('Invalid pre-period trade values; control selection halted.')
if (anyDuplicated(screen[c('reporter_iso','partner_iso','ref_year','hs2')]))
  stop('Duplicated reporter/partner/year/HS2 trade keys; control selection halted.')

# Exclude non-country partner aggregates and all EU27 suppliers. ISO3 validity is
# checked against the ISO country list; Kosovo XKX is handled explicitly below.
valid_iso <- unique(countrycode::codelist$iso3c)
valid_iso <- unique(c(valid_iso[!is.na(valid_iso)], 'XKX'))
valid_supplier <- screen$partner_iso %in% valid_iso &
                  !screen$partner_iso %in% EU27
s <- screen[valid_supplier, , drop=FALSE]
if (!'TUR' %in% s$partner_iso) stop('Turkey missing from eligible pre-period suppliers.')

# Exact denominator: observed imports from sovereign non-EU partners, for the
# 68 in-scope HS2 chapters. Unknown ROU cells remain unknown (never set to 0).
# Sum is across the SAME eligible supplier universe used in the numerator.
get_stats <- function(z) {
  tot <- sum(z$value)
  supplier_tot <- aggregate(value ~ partner_iso, data=z, FUN=sum)
  years <- aggregate(ref_year ~ partner_iso, data=unique(z[c('partner_iso','ref_year')]), FUN=length)
  names(years)[2] <- 'years_observed'
  reps <- aggregate(reporter_iso ~ partner_iso, data=unique(z[c('partner_iso','reporter_iso')]), FUN=length)
  names(reps)[2] <- 'importers_observed'
  hs <- aggregate(hs2 ~ partner_iso, data=unique(z[c('partner_iso','hs2')]), FUN=length)
  names(hs)[2] <- 'hs2_observed'
  out <- Reduce(function(a,b) merge(a,b,by='partner_iso',all=TRUE),
                list(supplier_tot,years,reps,hs))
  names(out)[names(out)=='value'] <- 'trade_value_usd'
  out$share <- out$trade_value_usd / tot
  out
}
stat27 <- get_stats(s)
stat26 <- get_stats(s[s$reporter_iso!='ROU', , drop=FALSE])
names(stat26)[-1] <- paste0(names(stat26)[-1], '_EU26')
sel <- merge(stat27, stat26, by='partner_iso', all.x=TRUE, sort=TRUE)
sel$eligible_EU27 <- with(sel, years_observed==5L & importers_observed>=20L &
                          hs2_observed>=50L & share>=0.001)
sel$eligible_EU26 <- with(sel, years_observed_EU26==5L & importers_observed_EU26>=20L &
                          hs2_observed_EU26>=50L & share_EU26>=0.001)
sel$screen_stable_EU26 <- sel$eligible_EU27 == sel$eligible_EU26
sel$share_threshold_crosses <- (sel$share>=0.001) != (sel$share_EU26>=0.001)
sel <- sel[order(-sel$share), ]
write.csv(sel, file.path(project_dir,'control_exante_screening.csv'), row.names=FALSE)

# CEPII weighted bilateral distances. CEPII file is an EXTERNAL prerequisite;
# never invent distances. Expected dist_cepii.xlsx in the project directory.
# Fixed reporter weights are 2015-2019 observed extra-EU imports, excluding
# unavailable ROU HS71/HS93; no post-period data enter weights.
cepii_path <- file.path(project_dir,'dist_cepii.xlsx')
if (!file.exists(cepii_path)) {
  cat('CONTROL SCREENING SAVED: control_exante_screening.csv\n')
  cat('DISTANCE STEP PENDING: put original CEPII dist_cepii.xlsx in project folder.\n')
  cat('NO FINAL CONTROL GROUP HAS BEEN SELECTED.\n')
} else {
  distances <- as.data.frame(readxl::read_excel(cepii_path))
  needed_dist <- c('iso_o','iso_d','distw')
  if (!all(needed_dist %in% names(distances)))
    stop('CEPII distance file must have iso_o, iso_d, distw columns.')
  distances$iso_o <- norm_iso(distances$iso_o)
  distances$iso_d <- norm_iso(distances$iso_d)
  distances$distw <- as.numeric(distances$distw)
  # Direction-agnostic matching; CEPII reports both origin and destination.
  distances$key <- paste(pmin(distances$iso_o,distances$iso_d),
                         pmax(distances$iso_o,distances$iso_d),sep='|')
  distances <- distances[!duplicated(distances$key),c('key','distw')]
  # Explicit geographic-code bridges (not trade-data recoding):
  # ROU -> ROM: CEPII historical Romania code.
  # SRB -> YUG: documented historical GeoDist proxy for Serbia.
  # CEPII Gravity (2022) also uses Yugoslavia distance values for Serbia.
  # Preserve partner_iso == 'SRB' in all outputs; proxy only for distance join.
  cei <- function(x) {
    x <- as.character(x)
    x[x == 'ROU'] <- 'ROM'
    x[x == 'SRB'] <- 'YUG'
    x
  }
  weights <- aggregate(value ~ reporter_iso, data=s, FUN=sum)
  names(weights)[2] <- 'weight_total'
  weights$weight <- weights$weight_total / sum(weights$weight_total)
  eligible <- sel$partner_iso[sel$eligible_EU27 | sel$partner_iso=='TUR']
  pair <- expand.grid(reporter_iso=EU27,partner_iso=eligible,stringsAsFactors=FALSE)
  pair$key <- paste(pmin(cei(pair$reporter_iso),cei(pair$partner_iso)),
                    pmax(cei(pair$reporter_iso),cei(pair$partner_iso)),sep='|')
  pair <- merge(pair,distances,by='key',all.x=TRUE,sort=FALSE)
  pair <- merge(pair,weights[c('reporter_iso','weight')],by='reporter_iso',all.x=TRUE)
  # Never silently reweight over missing bilateral distance pairs.
  if (anyNA(pair$distw) || any(!is.finite(pair$distw)) || any(pair$distw<=0)) {
    bad <- unique(pair[is.na(pair$distw) | !is.finite(pair$distw) | pair$distw<=0,
                       c('reporter_iso','partner_iso')])
    write.csv(bad,file.path(project_dir,'cepii_unmatched_pairs.csv'),row.names=FALSE)
    stop('Missing CEPII distances; see cepii_unmatched_pairs.csv. No final control group.')
  }
  if (anyNA(pair$weight)) stop('Missing reporter weights.')
  weighted <- aggregate(I(distw*weight) ~ partner_iso,data=pair,FUN=sum)
  names(weighted)[2] <- 'weighted_distw_km'
  turkey_km <- weighted$weighted_distw_km[weighted$partner_iso=='TUR']
  if (length(turkey_km)!=1L) stop('No unique Turkey distance baseline.')
  sel <- merge(sel,weighted,by='partner_iso',all.x=TRUE,sort=TRUE)
  sel$farther_than_TUR <- !is.na(sel$weighted_distw_km) &
                           sel$weighted_distw_km > turkey_km
  sel$distance_proxy <- ifelse(sel$partner_iso=='SRB','YUG_for_SRB', 'original_CEPII')
  sel$provisional_control <- sel$partner_iso!='TUR' &
                             sel$eligible_EU27 & sel$farther_than_TUR
  # If the missing Romania data could change the observed eligibility outcome,
  # do not finalize controls. AB26 stability is a diagnostic, not proof of no bias.
  uncertainty <- nrow(missing)>0L
  sel$selection_status <- if (uncertainty) 'PROVISIONAL_SOURCE_MISSING' else 'FINAL_IF_VALIDATED'
  write.csv(sel,file.path(project_dir,'control_exante_screening.csv'),row.names=FALSE)
  write.csv(sel[sel$provisional_control, , drop=FALSE],
            file.path(project_dir,'control_candidates_provisional.csv'),row.names=FALSE)
  cat('CEPII weighted Turkey distance:',round(turkey_km,2),'km\n')
  cat('Provisional controls:',sum(sel$provisional_control),'\n')
  cat('EU27/EU26 screening disagreements:',sum(!sel$screen_stable_EU26,na.rm=TRUE),'\n')
  cat('NO FINAL CONTROL GROUP: Romania source missingness requires assessment.\n')
}

# =============================================================================
# OPTIONAL 2020-2025 CONTROL-SUPPLIER ARCHIVE EXTENSION (download only)
# Run ONCE, with a valid personal UN Comtrade token already in your R session:
#   options(ibr.download_controls = TRUE)
#   source('C:/Users/rstat/Documents/Article_IBR_FINAL/01_data.R', encoding='UTF-8')
#   options(ibr.download_controls = FALSE)
# Existing 297 rawdata files are never changed. Downloaded files go to a
# SEPARATE staging folder and are not silently included in trade_data.rds.
# =============================================================================
if (isTRUE(getOption('ibr.download_controls', FALSE))) {
  if (!requireNamespace('comtradr', quietly=TRUE))
    stop('Install comtradr first: install.packages("comtradr")')
  if (as.character(utils::packageVersion('comtradr')) != '1.0.6')
    warning('The download was designed for comtradr 1.0.6; check API compatibility.')
  token <- Sys.getenv('COMTRADE_SECONDARY', unset='')
  if (!nzchar(token)) stop('COMTRADE_SECONDARY environment variable is empty. Do not put tokens in script files.')
  # comtradr 1.0.6 reads the registered primary key; ct_get_data() has no token argument.
  comtradr::set_primary_comtrade_key(token)
  cand_path <- file.path(project_dir, 'control_candidates_provisional.csv')
  if (!file.exists(cand_path)) stop('Missing control_candidates_provisional.csv.')
  cand <- utils::read.csv(cand_path, stringsAsFactors=FALSE)
  if (!all(c('partner_iso','provisional_control') %in% names(cand)))
    stop('Unexpected control candidate CSV structure.')
  candidates <- sort(unique(cand$partner_iso[cand$provisional_control %in% TRUE]))
  existing <- c('IND','JPN','KOR','THA')
  missing_partners <- setdiff(candidates, existing)
  if (length(candidates) != 31L || length(missing_partners) != 27L)
    stop('Expected 31 provisional controls and 27 new suppliers; inspect screening output.')
  extension_dir <- file.path(project_dir, 'rawdata_control_extension')
  dir.create(extension_dir, showWarnings=FALSE, recursive=TRUE)
  log_path <- file.path(extension_dir, 'download_log.csv')
  log_rows <- list()
  record <- function(reporter, yr, status, rows, note='') {
    log_rows[[length(log_rows)+1L]] <<- data.frame(
      reporter=reporter, year=yr, status=status, rows=rows,
      note=note, timestamp=format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
      stringsAsFactors=FALSE)
    utils::write.csv(do.call(rbind,log_rows),log_path,row.names=FALSE)
  }
  for (reporter in EU27) for (yr in 2020:2025) {
    target <- file.path(extension_dir, sprintf('%s_%d_HS2_extra.rds', reporter,yr))
    if (file.exists(target)) {
      old <- tryCatch(readRDS(target),error=function(e) NULL)
      if (is.data.frame(old) && all(c('reporter_iso','ref_year','partner_iso','cmd_code','primary_value') %in% names(old)) &&
          (nrow(old)==0L || (all(as.character(old$reporter_iso)==reporter) &&
                              all(as.integer(as.character(old$ref_year))==yr) &&
                              all(as.character(old$partner_iso) %in% missing_partners)))) {
        record(reporter,yr,'SKIP_EXISTING',nrow(old)); next
      }
      stop('Existing extension file failed validation: ',target)
    }
    cat('Downloading ',reporter,' ',yr,' (',length(missing_partners),' suppliers) ...\n',sep='')
    result <- NULL; last_error <- ''
    for (attempt in 1:5) {
      result <- tryCatch(comtradr::ct_get_data(
        reporter=reporter, partner=missing_partners,
        flow_direction='Import', commodity_classification='HS',
        commodity_code=HS68, start_date=yr, end_date=yr,
        frequency='A', verbose=FALSE, cache=FALSE),
        error=function(e) {last_error <<- conditionMessage(e); NULL})
      if (is.data.frame(result)) break
      Sys.sleep(min(180, 10 * 2^(attempt-1)))
    }
    if (!is.data.frame(result)) {
      record(reporter,yr,'FAILED',NA_integer_,last_error)
      stop('Download failed for ',reporter,' ',yr,': ',last_error,
           '. Re-run later; existing extension files will be preserved.')
    }
    needed <- c('reporter_iso','ref_year','partner_iso','cmd_code','aggr_level','flow_code','primary_value')
    if (!all(needed %in% names(result))) stop('Unexpected Comtrade schema: ',reporter,' ',yr)
    if (nrow(result)==0L) stop('Empty API result; not saved: ',reporter,' ',yr)
    if (nrow(result)>=100000L) stop('Potential API row limit/truncation: ',reporter,' ',yr)
    if (any(as.character(result$reporter_iso)!=reporter) ||
        any(as.integer(as.character(result$ref_year))!=yr) ||
        any(!as.character(result$partner_iso) %in% missing_partners) ||
        any(as.character(result$flow_code)!='M') ||
        any(as.integer(as.character(result$aggr_level))!=2L) ||
        anyNA(result$primary_value)) stop('API response validation failed: ',reporter,' ',yr)
    # The API can omit unreported HS2 cells; do not manufacture zeros.
    tmp <- paste0(target,'.partial')
    saveRDS(result,tmp,compress='xz')
    if (!file.rename(tmp,target)) stop('Unable to save: ',target)
    record(reporter,yr,'SAVED',nrow(result))
    Sys.sleep(3)
  }
  cat('CONTROL EXTENSION DOWNLOAD COMPLETE. Files: ',
      length(list.files(extension_dir,pattern='_extra\\.rds$')),
      '. Next step: validate and integrate without changing original archive.\n',sep='')
}

}

# =============================================================================
# H1 OPTIONAL STAGING (preview/pilot/download)
# =============================================================================
if (ibr_h1_mode %in% c('preview','pilot','download')) {
# H1 | RESUMABLE UN COMTRADE HS2 DOWNLOAD
# Integrated into 01_data.R; original archive remains untouched.
# comtradr 1.0.6; observed records only; NO zero imputation.
# Preview: options(ibr.h1.mode='preview'); source('C:/Users/rstat/Documents/Article_IBR_FINAL/01_data.R')
# Pilot: options(ibr.h1.mode='pilot'); source('C:/Users/rstat/Documents/Article_IBR_FINAL/01_data.R')
# Download: options(ibr.h1.mode='download'); source('C:/Users/rstat/Documents/Article_IBR_FINAL/01_data.R')

project_dir <- 'C:/Users/rstat/Documents/Article_IBR_FINAL'
stopifnot(dir.exists(project_dir))
needed_files <- c('H1_supplier_universe_audit.csv', 'H1_post_download_plan.csv',
                  'H1_CEPII_ONLY_MISSING.csv')
for (f in needed_files) if (!file.exists(file.path(project_dir, f)))
  stop('Required audit file missing: ', f)

sup <- read.csv(file.path(project_dir, needed_files[1]), stringsAsFactors=FALSE)
plan <- read.csv(file.path(project_dir, needed_files[2]), stringsAsFactors=FALSE)
miss <- read.csv(file.path(project_dir, needed_files[3]), stringsAsFactors=FALSE)
stopifnot(all(c('partner_iso','country_candidate','eu27_supplier') %in% names(sup)),
          all(c('reporter_iso','year') %in% names(plan)),
          all(c('importer','supplier') %in% names(miss)))

# Conservative, pre-specified H1 staging universe: CEPII-matched ISO3 codes.
# Excludes suppliers with ANY missing EU27 CEPII pair for now; this is a
# technical staging restriction, NOT a final empirical inclusion decision.
candidates <- sort(unique(toupper(trimws(sup$partner_iso[sup$country_candidate %in% TRUE]))))
cepii_missing <- sort(unique(toupper(trimws(miss$supplier))))
selected <- setdiff(candidates, cepii_missing)
# All 32 suppliers already downloaded for 2020-25 in the original + control extension.
existing32 <- c('TUR','BGD','CHN','HKG','IDN','IND','JPN','KHM','KOR','LKA',
                'MMR','MYS','PAK','PHL','SGP','THA','VNM','ARG','BRA','CAN',
                'CHL','MEX','USA','ISL','RUS','ARE','EGY','ISR','MAR','SAU','AUS','ZAF')
# Verify the already-downloaded control archive exists before omitting 27 controls.
control_dir <- file.path(project_dir, 'rawdata_control_extension')
original_dir <- file.path(project_dir, 'rawdata')
if (length(list.files(control_dir, pattern='_extra\\.rds$')) != 162L ||
    length(list.files(original_dir, pattern='\\.rds$')) != 297L)
  stop('Original/control archive file counts differ from 297/162; do not omit existing suppliers.')
new_suppliers <- setdiff(selected, existing32)
if (!length(new_suppliers)) stop('No new suppliers in staging universe.')
if (anyDuplicated(plan[c('reporter_iso','year')])) stop('Duplicate reporter-year in plan.')
if (nrow(plan) != 162L || !setequal(as.integer(plan$year), 2020:2025))
  stop('Unexpected plan coverage: review before download.')
if (!all(nchar(new_suppliers)==3L)) stop('Unexpected supplier ISO code.')

# Keep calls small enough to limit truncation risk; adjust downward if API fails.
batch_size <- getOption('ibr.h1.batch_size', 40L)
if (length(batch_size)!=1L || is.na(batch_size) || batch_size<1 || batch_size>100 || batch_size!=as.integer(batch_size))
  stop('ibr.h1.batch_size must be an integer from 1 to 100.')
chunks <- split(new_suppliers, ceiling(seq_along(new_suppliers)/batch_size))
work <- plan[order(plan$reporter_iso, plan$year), c('reporter_iso','year')]
work <- work[rep(seq_len(nrow(work)), each=length(chunks)), , drop=FALSE]
work$batch <- rep(seq_along(chunks), times=nrow(plan))
work$target <- sprintf('%s_%d_HS2_H1_batch%02d.rds',work$reporter_iso,work$year,work$batch)

stage <- file.path(project_dir,'rawdata_H1_extension')
# No directory is created in preview mode.
valid_saved <- function(path, reporter, year, allowed) {
  if (!file.exists(path)) return(FALSE)
  x <- tryCatch(readRDS(path), error=function(e) NULL)
  if (!is.data.frame(x) || !nrow(x)) return(FALSE)
  fields <- c('reporter_iso','ref_year','partner_iso','cmd_code','aggr_level','flow_code','primary_value')
  if (!all(fields %in% names(x))) return(FALSE)
  if (anyNA(x[c('reporter_iso','ref_year','partner_iso','cmd_code','primary_value')])) return(FALSE)
  all(as.character(x$reporter_iso)==reporter) &&
    all(as.integer(as.character(x$ref_year))==year) &&
    all(as.character(x$partner_iso) %in% allowed) &&
    all(as.character(x$flow_code)=='M') &&
    all(as.integer(as.character(x$aggr_level))==2L) &&
    all(is.finite(suppressWarnings(as.numeric(as.character(x$primary_value))))) &&
    !anyDuplicated(x[c('reporter_iso','partner_iso','ref_year','cmd_code')])
}
work$valid <- vapply(seq_len(nrow(work)), function(i) valid_saved(
  file.path(stage,work$target[i]),work$reporter_iso[i],work$year[i],chunks[[work$batch[i]]]),logical(1))
cat('\nH1 STAGING PREVIEW\n')
cat('Candidate ISO3:',length(candidates),'| CEPII-excluded suppliers:',length(cepii_missing),
    '| staging suppliers:',length(selected),'\n')
cat('Already covered suppliers excluded from new requests:',sum(selected %in% existing32),'\n')
cat('New suppliers:',length(new_suppliers),'| batch size:',batch_size,'\n')
cat('Total batch tasks:',nrow(work),'| saved valid:',sum(work$valid),
    '| remaining:',sum(!work$valid),'\n')
cat('Target directory:',stage,'\n')
if (identical(ibr_h1_mode, 'preview')) {
  cat('PREVIEW ONLY: no API requests or files created.\n')
} else {
  if (!requireNamespace('comtradr',quietly=TRUE)) stop('Install comtradr first.')
  if (as.character(utils::packageVersion('comtradr')) != '1.0.6')
    warning('Script prepared for comtradr 1.0.6; verify API compatibility.')
  key <- Sys.getenv('COMTRADE_SECONDARY',unset='')
  if (!nzchar(key)) key <- Sys.getenv('COMTRADE_PRIMARY',unset='')
  if (!nzchar(key)) stop('Set COMTRADE_SECONDARY or COMTRADE_PRIMARY in your R environment.')
  comtradr::set_primary_comtrade_key(key)
  dir.create(stage,recursive=TRUE,showWarnings=FALSE)
  # The counter is local to THIS script. Other API use is NOT counted.
  usage_file <- file.path(stage,'H1_api_attempts.csv')
  if (file.exists(usage_file)) {
    usage <- read.csv(usage_file,stringsAsFactors=FALSE)
    if (!all(c('date','time','reporter','year','batch','status','note') %in% names(usage)))
      stop('Unexpected existing API attempt log format.')
  } else {
    usage <- data.frame(date=character(),time=character(),reporter=character(),
                        year=integer(),batch=integer(),status=character(),note=character())
  }
  today <- format(Sys.Date(),'%Y-%m-%d')
  daily_budget <- getOption('ibr.h1.daily_budget',80L)
  if (length(daily_budget)!=1L || is.na(daily_budget) || daily_budget<1 ||
      daily_budget>500 || daily_budget!=as.integer(daily_budget))
    stop('ibr.h1.daily_budget must be an integer from 1 to 200.')
  attempts_today <- sum(usage$date==today & usage$status=='STARTED')
  HS68 <- sprintf('%02d',c(28:76,78:96))
  log_attempt <- function(reporter,year,batch,status,note='') {
    usage[nrow(usage)+1L,] <<- list(today,format(Sys.time(),'%Y-%m-%d %H:%M:%S'),
                                    reporter,as.integer(year),as.integer(batch),status,substr(note,1,500))
    write.csv(usage,usage_file,row.names=FALSE)
  }
  successful_this_run <- 0L
  for (i in which(!work$valid)) {
    if (attempts_today >= daily_budget) {
      cat('DAILY LOCAL BUDGET REACHED. Resume on another day.\n'); break
    }
    r <- work$reporter_iso[i]; y <- as.integer(work$year[i]); b <- work$batch[i]
    allowed <- setdiff(chunks[[b]],r) # Never query self-trade.
    if (!length(allowed)) next
    dest <- file.path(stage,work$target[i])
    if (file.exists(dest)) stop('Existing invalid file, manual review required: ',dest)
    cat('API request:',r,y,'batch',b,'| suppliers',length(allowed),'\n')
    # Log BEFORE request so interrupted sessions do not undercount.
    log_attempt(r,y,b,'STARTED')
    attempts_today <- attempts_today+1L
    result <- tryCatch(comtradr::ct_get_data(
      reporter=r,partner=allowed,flow_direction='Import',
      commodity_classification='HS',commodity_code=HS68,
      start_date=y,end_date=y,frequency='A',verbose=FALSE,cache=FALSE),
      error=function(e) e)
    if (inherits(result,'error')) {
      log_attempt(r,y,b,'ERROR',conditionMessage(result))
      stop('API error; no file saved: ',conditionMessage(result),
           '\nResolve the issue, then re-run; valid files will be skipped.')
    }
    if (!is.data.frame(result) || nrow(result)==0L || nrow(result)>=100000L) {
      log_attempt(r,y,b,'REVIEW','Empty/non-data.frame/possible row limit')
      stop('Response needs review: ',r,' ',y,' batch ',b,'; nothing saved.')
    }
    fields <- c('reporter_iso','ref_year','partner_iso','cmd_code','aggr_level','flow_code','primary_value')
    # Validate in memory before writing.
    ok <- all(fields %in% names(result))
    if (ok) ok <- !anyNA(result[fields]) &&
      all(as.character(result$reporter_iso)==r) &&
      all(as.integer(as.character(result$ref_year))==y) &&
      all(as.character(result$partner_iso) %in% allowed) &&
      all(as.character(result$flow_code)=='M') &&
      all(as.integer(as.character(result$aggr_level))==2L) &&
      all(as.character(result$cmd_code) %in% HS68) &&
      all(is.finite(suppressWarnings(as.numeric(as.character(result$primary_value))))) &&
      !anyDuplicated(result[c('reporter_iso','partner_iso','ref_year','cmd_code')])
    if (!ok) {
      log_attempt(r,y,b,'REVIEW','Schema/content validation failed')
      stop('Invalid response; not saved: ',r,' ',y,' batch ',b)
    }
    temp <- paste0(dest,'.partial')
    saveRDS(result,temp,compress='xz')
    if (!file.rename(temp,dest)) stop('Could not finalize file: ',dest)
    log_attempt(r,y,b,'SAVED',paste('rows',nrow(result)))
    cat('Saved:',basename(dest),'rows:',nrow(result),'\n')
    successful_this_run <- successful_this_run + 1L
    if (identical(ibr_h1_mode, 'pilot')) {
      cat('PILOT COMPLETE: one validated batch saved. Review before full download.\n')
      break
    }
    Sys.sleep(3)
  }
  cat('H1 run finished. Re-source in preview mode to see remaining tasks.\n')
}

}


}

# =============================================================================
# ACTIVE INTEGRATION | ONE LOCAL RAWDATA FOLDER | NO NETWORK OPERATIONS
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

ibr_h1_mode <- getOption("ibr.h1.mode", "integrate")
if (!identical(ibr_h1_mode, "integrate")) {
  stop("Historical modes are disabled. Use options(ibr.h1.mode='integrate') or remove the option.")
}
# =============================================================================
# INTEGRATE MODE: LOCAL ARCHIVES ONLY; NO API CALLS; NO RAW FILE MODIFICATIONS
# Usage:
#   options(ibr.h1.mode = 'integrate')
#   source('01_data.R', encoding = 'UTF-8')
#
# Creates a COMPACT observed-trade archive and QA files. It does NOT replace
# trade_data.rds, select a final estimation sample, or fill missing cells.
# =============================================================================
if (identical(ibr_h1_mode, 'integrate')) {
  project_dir <- ibr_project_dir("01_data.R")
  raw_dir <- file.path(project_dir, "rawdata")
  if (!dir.exists(raw_dir)) stop("Missing rawdata folder: ", raw_dir)
  all_files <- sort(list.files(raw_dir, pattern="\\.rds$", full.names=TRUE,
                               ignore.case=TRUE, recursive=FALSE))
  if (length(all_files) != 1269L)
    stop("Expected 1269 RDS files in rawdata; found ", length(all_files),
         ". No output written.")
  fn <- basename(all_files)
  original_files <- grepl("^[A-Z]{3}_20[0-9]{2}_HS2(_selected)?\\.rds$", fn)
  control_files <- grepl("^[A-Z]{3}_20[0-9]{2}_HS2_extra\\.rds$", fn)
  h1_files <- grepl("^[A-Z]{3}_20[0-9]{2}_HS2_H1_batch[0-9]{2}\\.rds$", fn)
  if (any(!(original_files | control_files | h1_files)))
    stop("Unrecognized RDS filenames: ",
         paste(fn[!(original_files | control_files | h1_files)], collapse=", "))
  if (anyDuplicated(tolower(fn)))
    stop("Case-insensitive duplicate RDS filenames found.")
  folders <- c("rawdata", "rawdata_control_extension", "rawdata_H1_extension")
  # Preserve the historical provenance labels and concatenation order.
  source_folders <- ifelse(original_files, folders[1],
                           ifelse(control_files, folders[2], folders[3]))
  order_idx <- order(match(source_folders, folders), fn)
  paths <- all_files[order_idx]
  source_folders <- source_folders[order_idx]
  counts <- as.integer(table(factor(source_folders, levels=folders)))
  if (!identical(counts, c(297L, 162L, 810L)))
    stop("Unexpected original/control/H1 counts: ",
         paste(counts, collapse="/"), "; expected 297/162/810. No output written.")
  cat("OFFLINE INPUT VERIFIED: ", length(paths), " RDS files (",
      paste(counts, collapse="/"), ").\\n", sep="")
  required <- c('reporter_iso','partner_iso','ref_year','cmd_code',
                'aggr_level','flow_code','primary_value',
                'classification_code','is_aggregate','is_reported')
  keep <- required
  chunks <- vector('list', length(paths))
  file_checks <- vector('list', length(paths))
  schema_reference <- NULL
  expected_hs <- sprintf('%02d', c(28:76, 78:96))

  for (i in seq_along(paths)) {
    x <- tryCatch(readRDS(paths[i]),
                  error=function(e) stop('Unreadable RDS: ', paths[i], ': ',
                                         conditionMessage(e)))
    if (!is.data.frame(x)) stop('Not a data frame: ', paths[i])
    if (is.null(schema_reference)) schema_reference <- names(x)
    if (!identical(names(x), schema_reference))
      stop('Different column structure: ', paths[i])
    if (!all(required %in% names(x)))
      stop('Missing mandatory fields: ', paths[i])

    filename <- basename(paths[i])
    folder <- source_folders[i]
    pat <- switch(folder,
      rawdata='^([A-Z]{3})_(20[0-9]{2})_HS2(_selected)?\\.rds$',
      rawdata_control_extension='^([A-Z]{3})_(20[0-9]{2})_HS2_extra\\.rds$',
      rawdata_H1_extension='^([A-Z]{3})_(20[0-9]{2})_HS2_H1_batch[0-9]{2}\\.rds$')
    match <- regmatches(filename, regexec(pat, filename))[[1]]
    if (!length(match)) stop('Unexpected filename: ', filename)
    reporter <- match[2]
    year <- as.integer(match[3])

    if (folder != 'rawdata' && !(year %in% 2020:2025))
      stop('Extension year outside 2020-2025: ', filename)
    if (folder == 'rawdata' &&
        (grepl('_selected\\.rds$', filename) != (year >= 2020L)))
      stop('Original filename/year inconsistency: ', filename)

    z <- x[keep]
    z$reporter_iso <- toupper(trimws(as.character(z$reporter_iso)))
    z$partner_iso <- toupper(trimws(as.character(z$partner_iso)))
    z$ref_year <- suppressWarnings(as.integer(as.character(z$ref_year)))
    z$cmd_code <- as.character(z$cmd_code)
    z$aggr_level <- suppressWarnings(as.integer(as.character(z$aggr_level)))
    z$flow_code <- as.character(z$flow_code)
    z$primary_value <- suppressWarnings(as.numeric(as.character(z$primary_value)))
    z$classification_code <- as.character(z$classification_code)

    if (nrow(z) == 0L) stop('Unexpected empty file: ', filename)
    if (anyNA(z[c('reporter_iso','partner_iso','ref_year','cmd_code',
                  'aggr_level','flow_code','primary_value')]))
      stop('Missing mandatory observation values: ', filename)
    if (any(z$reporter_iso != reporter) || any(z$ref_year != year))
      stop('Filename and row metadata disagree: ', filename)
    if (any(z$flow_code != 'M') || any(z$aggr_level != 2L))
      stop('Unexpected flow or aggregation: ', filename)
    if (any(!z$cmd_code %in% expected_hs))
      stop('Unexpected HS2 chapter: ', filename)
    if (any(!is.finite(z$primary_value) | z$primary_value < 0))
      stop('Invalid trade value: ', filename)

    z$source_folder <- folder
    z$source_file <- filename
    chunks[[i]] <- z
    file_checks[[i]] <- data.frame(
      source_folder=folder, source_file=filename,
      reporter_iso=reporter, ref_year=year, rows=nrow(z),
      classification=paste(sort(unique(z$classification_code)),collapse=';'),
      partners=length(unique(z$partner_iso)),
      stringsAsFactors=FALSE)
  }

  cat('All files read. Combining observed records...\n')
  trade_integrated <- do.call(rbind, chunks)
  rownames(trade_integrated) <- NULL
  rm(chunks)
  gc(verbose=FALSE)

  # Trade key is defined at reporter-partner-year-HS2 level. No silent dedup.
  key_cols <- c('reporter_iso','partner_iso','ref_year','cmd_code')
  duplicate <- duplicated(trade_integrated[key_cols]) |
               duplicated(trade_integrated[key_cols], fromLast=TRUE)
  if (any(duplicate)) {
    bad <- trade_integrated[duplicate,
      c(key_cols,'classification_code','source_folder','source_file')]
    stop('Duplicate trade keys found (', nrow(bad),
         ' affected rows). Nothing written; inspect input files.')
  }

  audit_files <- do.call(rbind, file_checks)
  audit_class <- as.data.frame(table(
    year=trade_integrated$ref_year,
    classification=trade_integrated$classification_code),
    stringsAsFactors=FALSE)
  audit_class <- audit_class[audit_class$Freq > 0L, ]
  audit_coverage <- aggregate(
    cmd_code ~ reporter_iso + ref_year,
    data=trade_integrated,
    FUN=function(x) length(unique(x)))
  names(audit_coverage)[3] <- 'distinct_hs2'

  # Pre/post source coverage is deliberately NOT interpreted as a balanced panel.
  # Validate against the archived integration result before replacing any output.
  if (nrow(trade_integrated) != 1499841L)
    stop("Unexpected integrated row count: ", nrow(trade_integrated),
         "; expected 1499841. Existing output was not changed.")
  output <- file.path(project_dir, 'trade_data_integrated.rds')
  output_temp <- paste0(output, '.partial')
  if (file.exists(output_temp))
    stop('Existing partial output found; inspect before retrying: ', output_temp)
  saveRDS(trade_integrated, output_temp, compress='xz')
  if (!file.rename(output_temp, output))
    stop('Could not finalize integrated RDS. Partial output: ', output_temp)

  write.csv(audit_files,
            file.path(project_dir,'integrated_file_audit.csv'),
            row.names=FALSE)
  write.csv(audit_class,
            file.path(project_dir,'integrated_hs_vintage_audit.csv'),
            row.names=FALSE)
  write.csv(audit_coverage,
            file.path(project_dir,'integrated_coverage_audit.csv'),
            row.names=FALSE)

  lines <- c(
    'RETHINKING NEARSHORING | INTEGRATED OBSERVED TRADE AUDIT',
    paste('Created:', format(Sys.time(),'%Y-%m-%d %H:%M:%S %Z')),
    paste('Input files:', length(paths)),
    paste('Original/control/H1:', paste(counts, collapse='/')),
    paste('Observed rows:', nrow(trade_integrated)),
    paste('Duplicate reporter-partner-year-HS2 keys:', sum(duplicate)),
    paste('Years:', paste(sort(unique(trade_integrated$ref_year)), collapse=', ')),
    paste('Reporters:', length(unique(trade_integrated$reporter_iso))),
    paste('Supplier codes (including aggregates):',
          length(unique(trade_integrated$partner_iso))),
    'No missing trade values were imputed as zero.',
    'HS vintages are retained; this script does not harmonize H4/H5/H6.',
    'CEPII distances and final sample selection require separate validation.',
    'Original raw RDS files and trade_data.rds were not modified.',
    'This compact file retains 10 source fields plus source_folder/source_file.',
    'No API requests were sent.'
  )
  writeLines(lines, file.path(project_dir,'integrated_data_audit.txt'))
  cat('\nINTEGRATION SUCCESS\n')
  cat('Files:', length(paths), '| Rows:', nrow(trade_integrated),
      '| Duplicate keys:', sum(duplicate), '\n')
  cat('Output:', output, '\n')
  cat('QA: integrated_file_audit.csv, integrated_hs_vintage_audit.csv,\n')
  cat('    integrated_coverage_audit.csv, integrated_data_audit.txt\n')
}
