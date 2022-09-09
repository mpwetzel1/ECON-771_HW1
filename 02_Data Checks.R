

# Locate and review parsing problems from HCRIS v 1996 import
for (i in 2000:2011) {
  print(paste0("Year is ", i))
  HCRIS.alpha=read_csv(paste0("./Data/HCRIS_v1996/HospitalFY",i,"/hosp_",i,"_ALPHA.CSV"),
                       col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.numeric=read_csv(paste0("./Data/HCRIS_v1996/HospitalFY",i,"/hosp_",i,"_NMRC.CSV"),
                         col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.report=read_csv(paste0("./Data/HCRIS_v1996/HospitalFY",i,"/hosp_",i,"_RPT.CSV"),
                        col_names=c('RPT_REC_NUM','PRVDR_CTRL_TYPE_CD','PRVDR_NUM','NPI',
                                    'RPT_STUS_CD','FY_BGN_DT','FY_END_DT','PROC_DT',
                                    'INITL_RPT_SW','LAST_RPT_SW','TRNSMTL_NUM','FI_NUM',
                                    'ADR_VNDR_CD','FI_CREAT_DT','UTIL_CD','NPR_DT',
                                    'SPEC_IND','FI_RCPT_DT'))

  
  stop_for_problems(HCRIS.alpha)
  stop_for_problems(HCRIS.numeric)
  stop_for_problems(HCRIS.report)
  
  
}

problems(HCRIS.alpha)
problems(HCRIS.numeric)
problems(HCRIS.report)

## Conclude: It's two lines from 1999, not worth correcting for a homework assignment