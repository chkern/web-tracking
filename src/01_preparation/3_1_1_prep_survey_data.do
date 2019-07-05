*Preparation of survey data
clear all
usespss "Y:\\Respondi\\RESPONDI_w3\\survey_daten\\6423_BTW_Sinus_final_W1+W2+W3_2017_10_06.sav"

*There seems to be some importing issue from SPSS file to Stata file
foreach v of varlist * {
recode `v' (.a=-77)
}

rename pseudonym panelist_id
rename m_6001 sinus_submil
rename m_6000 sinus_mil
rename sample_w2 refresh_samp_w2
//Gender
gen male =1 if v_7==1
replace male =0 if v_7==2
drop v_7

//Age
rename v_8 age
rename age_reco age_cat

//federal state
rename v_9 f_state
gen west_ger =1 if f_state==1| f_state==2| f_state==5| f_state==6| f_state==7| ///
f_state==9| f_state==10| f_state==11| f_state==12| f_state==15
recode west_ger (.=0)

//education
rename v_10 education
rename v_11 edu_cat

//occupation and prestige
gen employed=1 if v_407==1
recode employed (.=0)
drop v_407

rename v_408 occ_pos
rename v_409 why_unemp

replace why_unemp = -77 if occ_pos>0 & occ_pos!=.
replace occ_pos = -78 if occ_pos==.


//income
rename v_410 inc_cat

//size of municipality
rename v_411 size_hometown

//householdsize
rename v_412 hh_size

//children?
drop v_413
recode v_414 (-99=0) (-66=.)
rename v_414 children

gen child_in_hh=1 if v_415==1
recode child_in_hh (.=0) if v_415==2
drop v_415 

rename v_416 num_child_hh
recode  num_child_hh (-99=0) (-66=.)

foreach x of numlist 1/2 {
	rename v_42_w`x'  q1_w`x'
	rename v_43_w`x'  q2_1_w`x'
	rename v_44_w`x'  q2_2_w`x'
	rename v_45_w`x'  q2_3_w`x'
	rename v_46_w`x'  q2_4_w`x'
	rename v_47_w`x'  q2_5_w`x'
	rename v_48_w`x'  q2_6_w`x'
	rename v_49_w`x'  q2_7_w`x'
	rename v_50_w`x'  q2_8_w`x'
	rename v_51_w`x'  q2_9_w`x'
	rename v_52_w`x'  q2_10_w`x'
	rename v_53_w`x'  q2_11_w`x'
	rename v_54_w`x'  q2_12_w`x'
	rename v_55_w`x'  q2_13_w`x'
	rename v_56_w`x'  q2_14_w`x'
	rename v_57_w`x'  q2_15_w`x'
	rename v_58_w`x'  q2_16_w`x'
	rename v_60_w`x'  q2_17_w`x'
	rename v_350_w`x' q7_w`x'
	rename v_353_w`x' q8_w`x'
	rename v_354_w`x' q9_CDU_w`x'
	rename v_355_w`x' q9_SPD_w`x'
	rename v_356_w`x' q9_B90_w`x'
	rename v_357_w`x' q9_FDP_w`x'
	rename v_358_w`x' q9_LIN_w`x'
	rename v_359_w`x' q9_AFD_w`x'
	rename v_360_w`x' q9_oth_w`x'
	rename v_361_w`x' q9_inv_w`x'
	rename v_362_w`x' q9_non_w`x'
	rename v_363_w`x' q9_dk_w`x'
	rename v_364_w`x' q10_CDU_w`x'
	rename v_365_w`x' q10_SPD_w`x'
	rename v_366_w`x' q10_B90_w`x'
	rename v_367_w`x' q10_FDP_w`x'
	rename v_368_w`x' q10_LIN_w`x'
	rename v_369_w`x' q10_AFD_w`x'
	rename v_370_w`x' q10_oth_w`x'
	rename v_371_w`x' q10_inv_w`x'
	rename v_372_w`x' q10_all_w`x'
	rename v_373_w`x' q10_dk_w`x'
	rename v_374_w`x' q11_w`x'
	rename v_375_w`x' q12_w`x'
	rename v_376_w`x' q13_CDU_w`x'
	rename v_377_w`x' q13_SPD_w`x'
	rename v_378_w`x' q13_B90_w`x'
	rename v_379_w`x' q13_FDP_w`x'
	rename v_380_w`x' q13_LIN_w`x'
	rename v_381_w`x' q13_AFD_w`x'
	rename v_382_w`x' q13_oth_w`x'
	rename v_383_w`x' q13_inv_w`x'
	rename v_384_w`x' q13_all_w`x'
	rename v_385_w`x' q13_dk_w`x'
	rename v_386_w`x' q14_CDU_w`x'
	rename v_387_w`x' q14_SPD_w`x'
	rename v_388_w`x' q14_B90_w`x'
	rename v_389_w`x' q14_FDP_w`x'
	rename v_390_w`x' q14_LIN_w`x'
	rename v_391_w`x' q14_AFD_w`x'
	rename v_392_w`x' q14_oth_w`x'
	rename v_393_w`x' q14_inv_w`x'
	rename v_394_w`x' q14_all_w`x'
	rename v_395_w`x' q14_dk_w`x'
	rename v_396_w`x' q15_CDU_w`x'
	rename v_397_w`x' q15_SPD_w`x'
	rename v_398_w`x' q15_B90_w`x'
	rename v_399_w`x' q15_FDP_w`x'
	rename v_400_w`x' q15_LIN_w`x'
	rename v_401_w`x' q15_AFD_w`x'
	rename v_402_w`x' q16_w`x'
	rename v_403_w`x' q17_w`x'
	rename v_404_w`x' q18_w`x'
}

rename v_405_w1 q19_w1
rename v_421_w2 q20_w2
rename v_420_w2 q21_w2
rename v_406_w2 q22_w2
rename v_422_w2 q23_w2

	
foreach x of numlist 1/3 {
	rename v_61_w`x' q3_AM_w`x'
	rename v_62_w`x' q3_MS_w`x'
	rename v_63_w`x' q3_JH_w`x'
	rename v_64_w`x' q3_SW_w`x'
	rename v_65_w`x' q3_DB_w`x'
	rename v_66_w`x' q3_KGE_w`x'
	rename v_67_w`x' q3_COZ_w`x'
	rename v_68_w`x' q3_CL_w`x'
	rename v_69_w`x' q3_AW_w`x'
	rename v_70_w`x' q3_AG_w`x'
	rename v_71_w`x' q4_1_AM_w`x'
	rename v_72_w`x' q4_1_MS_w`x'
	rename v_73_w`x' q4_1_JH_w`x'
	rename v_74_w`x' q4_1_SW_w`x'
	rename v_75_w`x' q4_1_DB_w`x'
	rename v_76_w`x' q4_1_KGE_w`x'
	rename v_77_w`x' q4_1_COZ_w`x'
	rename v_78_w`x' q4_1_CL_w`x'
	rename v_79_w`x' q4_1_AW_w`x'
	rename v_80_w`x' q4_1_AG_w`x'
	rename v_81_w`x' q4_2_AM_w`x'
	rename v_82_w`x' q4_2_MS_w`x'
	rename v_83_w`x' q4_2_JH_w`x'
	rename v_84_w`x' q4_2_SW_w`x'
	rename v_85_w`x' q4_2_DB_w`x'
	rename v_86_w`x' q4_2_KGE_w`x'
	rename v_87_w`x' q4_2_COZ_w`x'
	rename v_88_w`x' q4_2_CL_w`x'
	rename v_89_w`x' q4_2_AW_w`x'
	rename v_90_w`x' q4_2_AG_w`x'
	rename v_91_w`x' q4_3_AM_w`x'
	rename v_92_w`x' q4_3_MS_w`x'
	rename v_93_w`x' q4_3_JH_w`x'
	rename v_94_w`x' q4_3_SW_w`x'
	rename v_95_w`x' q4_3_DB_w`x'
	rename v_96_w`x' q4_3_KGE_w`x'
	rename v_97_w`x' q4_3_COZ_w`x'
	rename v_98_w`x' q4_3_CL_w`x'
	rename v_99_w`x' q4_3_AW_w`x'
	rename v_100_w`x' q4_3_AG_w`x'
	rename v_101_w`x' q4_4_AM_w`x'
	rename v_102_w`x' q4_4_MS_w`x'
	rename v_103_w`x' q4_4_JH_w`x'
	rename v_104_w`x' q4_4_SW_w`x'
	rename v_105_w`x' q4_4_DB_w`x'
	rename v_106_w`x' q4_4_KGE_w`x'
	rename v_107_w`x' q4_4_COZ_w`x'
	rename v_108_w`x' q4_4_CL_w`x'
	rename v_109_w`x' q4_4_AW_w`x'
	rename v_110_w`x' q4_4_AG_w`x'
	rename v_111_w`x' q4_5_AM_w`x'
	rename v_112_w`x' q4_5_MS_w`x'
	rename v_113_w`x' q4_5_JH_w`x'
	rename v_114_w`x' q4_5_SW_w`x'
	rename v_115_w`x' q4_5_DB_w`x'
	rename v_116_w`x' q4_5_KGE_w`x'
	rename v_117_w`x' q4_5_COZ_w`x'
	rename v_118_w`x' q4_5_CL_w`x'
	rename v_119_w`x' q4_5_AW_w`x'
	rename v_120_w`x' q4_5_AG_w`x'
	rename v_121_w`x' q4_6_AM_w`x'
	rename v_122_w`x' q4_6_MS_w`x'
	rename v_123_w`x' q4_6_JH_w`x'
	rename v_124_w`x' q4_6_SW_w`x'
	rename v_125_w`x' q4_6_DB_w`x'
	rename v_126_w`x' q4_6_KGE_w`x'
	rename v_127_w`x' q4_6_COZ_w`x'
	rename v_128_w`x' q4_6_CL_w`x'
	rename v_129_w`x' q4_6_AW_w`x'
	rename v_130_w`x' q4_6_AG_w`x'
	rename v_131_w`x' q4_7_AM_w`x'
	rename v_132_w`x' q4_7_MS_w`x'
	rename v_133_w`x' q4_7_JH_w`x'
	rename v_134_w`x' q4_7_SW_w`x'
	rename v_135_w`x' q4_7_DB_w`x'
	rename v_136_w`x' q4_7_KGE_w`x'
	rename v_137_w`x' q4_7_COZ_w`x'
	rename v_138_w`x' q4_7_CL_w`x'
	rename v_139_w`x' q4_7_AW_w`x'
	rename v_140_w`x' q4_7_AG_w`x'
	rename v_141_w`x' q4_8_AM_w`x'
	rename v_142_w`x' q4_8_MS_w`x'
	rename v_143_w`x' q4_8_JH_w`x'
	rename v_144_w`x' q4_8_SW_w`x'
	rename v_145_w`x' q4_8_DB_w`x'
	rename v_146_w`x' q4_8_KGE_w`x'
	rename v_147_w`x' q4_8_COZ_w`x'
	rename v_148_w`x' q4_8_CL_w`x'
	rename v_149_w`x' q4_8_AW_w`x'
	rename v_150_w`x' q4_8_AG_w`x'
	rename v_151_w`x' q4_9_AM_w`x'
	rename v_152_w`x' q4_9_MS_w`x'
	rename v_153_w`x' q4_9_JH_w`x'
	rename v_154_w`x' q4_9_SW_w`x'
	rename v_155_w`x' q4_9_DB_w`x'
	rename v_156_w`x' q4_9_KGE_w`x'
	rename v_157_w`x' q4_9_COZ_w`x'
	rename v_158_w`x' q4_9_CL_w`x'
	rename v_159_w`x' q4_9_AW_w`x'
	rename v_160_w`x' q4_9_AG_w`x'
	rename v_161_w`x' q4_10_AM_w`x'
	rename v_162_w`x' q4_10_MS_w`x'
	rename v_163_w`x' q4_10_JH_w`x'
	rename v_164_w`x' q4_10_SW_w`x'
	rename v_165_w`x' q4_10_DB_w`x'
	rename v_166_w`x' q4_10_KGE_w`x'
	rename v_167_w`x' q4_10_COZ_w`x'
	rename v_168_w`x' q4_10_CL_w`x'
	rename v_169_w`x' q4_10_AW_w`x'
	rename v_170_w`x' q4_10_AG_w`x'
	rename v_171_w`x' q4_11_AM_w`x'
	rename v_172_w`x' q4_11_MS_w`x'
	rename v_173_w`x' q4_11_JH_w`x'
	rename v_174_w`x' q4_11_SW_w`x'
	rename v_175_w`x' q4_11_DB_w`x'
	rename v_176_w`x' q4_11_KGE_w`x'
	rename v_177_w`x' q4_11_COZ_w`x'
	rename v_178_w`x' q4_11_CL_w`x'
	rename v_179_w`x' q4_11_AW_w`x'
	rename v_180_w`x' q4_11_AG_w`x'
	rename v_181_w`x' q4_12_AM_w`x'
	rename v_182_w`x' q4_12_MS_w`x'
	rename v_183_w`x' q4_12_JH_w`x'
	rename v_184_w`x' q4_12_SW_w`x'
	rename v_185_w`x' q4_12_DB_w`x'
	rename v_186_w`x' q4_12_KGE_w`x'
	rename v_187_w`x' q4_12_COZ_w`x'
	rename v_188_w`x' q4_12_CL_w`x'
	rename v_189_w`x' q4_12_AW_w`x'
	rename v_190_w`x' q4_12_AG_w`x'
	rename v_191_w`x' q4_13_AM_w`x'
	rename v_192_w`x' q4_13_MS_w`x'
	rename v_193_w`x' q4_13_JH_w`x'
	rename v_194_w`x' q4_13_SW_w`x'
	rename v_195_w`x' q4_13_DB_w`x'
	rename v_196_w`x' q4_13_KGE_w`x'
	rename v_197_w`x' q4_13_COZ_w`x'
	rename v_198_w`x' q4_13_CL_w`x'
	rename v_199_w`x' q4_13_AW_w`x'
	rename v_200_w`x' q4_13_AG_w`x'
	rename v_201_w`x' q4_14_AM_w`x'
	rename v_202_w`x' q4_14_MS_w`x'
	rename v_203_w`x' q4_14_JH_w`x'
	rename v_204_w`x' q4_14_SW_w`x'
	rename v_205_w`x' q4_14_DB_w`x'
	rename v_206_w`x' q4_14_KGE_w`x'
	rename v_207_w`x' q4_14_COZ_w`x'
	rename v_208_w`x' q4_14_CL_w`x'
	rename v_209_w`x' q4_14_AW_w`x'
	rename v_210_w`x' q4_14_AG_w`x'
	rename v_211_w`x' q4_15_AM_w`x'
	rename v_212_w`x' q4_15_MS_w`x'
	rename v_213_w`x' q4_15_JH_w`x'
	rename v_214_w`x' q4_15_SW_w`x'
	rename v_215_w`x' q4_15_DB_w`x'
	rename v_216_w`x' q4_15_KGE_w`x'
	rename v_217_w`x' q4_15_COZ_w`x'
	rename v_218_w`x' q4_15_CL_w`x'
	rename v_219_w`x' q4_15_AW_w`x'
	rename v_220_w`x' q4_15_AG_w`x'
	rename v_221_w`x' q4_16_AM_w`x'
	rename v_222_w`x' q4_16_MS_w`x'
	rename v_223_w`x' q4_16_JH_w`x'
	rename v_224_w`x' q4_16_SW_w`x'
	rename v_225_w`x' q4_16_DB_w`x'
	rename v_226_w`x' q4_16_KGE_w`x'
	rename v_227_w`x' q4_16_COZ_w`x'
	rename v_228_w`x' q4_16_CL_w`x'
	rename v_229_w`x' q4_16_AW_w`x'
	rename v_230_w`x' q4_16_AG_w`x'
	rename v_231_w`x' q5_CDU_w`x'
	rename v_232_w`x' q5_CSU_w`x'
	rename v_233_w`x' q5_SPD_w`x'
	rename v_234_w`x' q5_B90_w`x'
	rename v_235_w`x' q5_FDP_w`x'
	rename v_236_w`x' q5_LIN_w`x'
	rename v_237_w`x' q5_AFD_w`x'
	rename v_238_w`x' q6_1_CDU_w`x'
	rename v_239_w`x' q6_1_CSU_w`x'
	rename v_240_w`x' q6_1_SPD_w`x'
	rename v_241_w`x' q6_1_B90_w`x'
	rename v_242_w`x' q6_1_FDP_w`x'
	rename v_243_w`x' q6_1_LIN_w`x'
	rename v_244_w`x' q6_1_AFD_w`x'
	rename v_245_w`x' q6_2_CDU_w`x'
	rename v_246_w`x' q6_2_CSU_w`x'
	rename v_247_w`x' q6_2_SPD_w`x'
	rename v_248_w`x' q6_2_B90_w`x'
	rename v_249_w`x' q6_2_FDP_w`x'
	rename v_250_w`x' q6_2_LIN_w`x'
	rename v_251_w`x' q6_2_AFD_w`x'
	rename v_252_w`x' q6_3_CDU_w`x'
	rename v_253_w`x' q6_3_CSU_w`x'
	rename v_254_w`x' q6_3_SPD_w`x'
	rename v_255_w`x' q6_3_B90_w`x'
	rename v_256_w`x' q6_3_FDP_w`x'
	rename v_257_w`x' q6_3_LIN_w`x'
	rename v_258_w`x' q6_3_AFD_w`x'
	rename v_259_w`x' q6_4_CDU_w`x'
	rename v_260_w`x' q6_4_CSU_w`x'
	rename v_261_w`x' q6_4_SPD_w`x'
	rename v_262_w`x' q6_4_B90_w`x'
	rename v_263_w`x' q6_4_FDP_w`x'
	rename v_264_w`x' q6_4_LIN_w`x'
	rename v_265_w`x' q6_4_AFD_w`x'
	rename v_266_w`x' q6_5_CDU_w`x'
	rename v_267_w`x' q6_5_CSU_w`x'
	rename v_268_w`x' q6_5_SPD_w`x'
	rename v_269_w`x' q6_5_B90_w`x'
	rename v_270_w`x' q6_5_FDP_w`x'
	rename v_271_w`x' q6_5_LIN_w`x'
	rename v_272_w`x' q6_5_AFD_w`x'
	rename v_273_w`x' q6_6_CDU_w`x'
	rename v_274_w`x' q6_6_CSU_w`x'
	rename v_275_w`x' q6_6_SPD_w`x'
	rename v_276_w`x' q6_6_B90_w`x'
	rename v_277_w`x' q6_6_FDP_w`x'
	rename v_278_w`x' q6_6_LIN_w`x'
	rename v_279_w`x' q6_6_AFD_w`x'
	rename v_280_w`x' q6_7_CDU_w`x'
	rename v_281_w`x' q6_7_CSU_w`x'
	rename v_282_w`x' q6_7_SPD_w`x'
	rename v_283_w`x' q6_7_B90_w`x'
	rename v_284_w`x' q6_7_FDP_w`x'
	rename v_285_w`x' q6_7_LIN_w`x'
	rename v_286_w`x' q6_7_AFD_w`x'
	rename v_287_w`x' q6_8_CDU_w`x'
	rename v_288_w`x' q6_8_CSU_w`x'
	rename v_289_w`x' q6_8_SPD_w`x'
	rename v_290_w`x' q6_8_B90_w`x'
	rename v_291_w`x' q6_8_FDP_w`x'
	rename v_292_w`x' q6_8_LIN_w`x'
	rename v_293_w`x' q6_8_AFD_w`x'
	rename v_294_w`x' q6_9_CDU_w`x'
	rename v_295_w`x' q6_9_CSU_w`x'
	rename v_296_w`x' q6_9_SPD_w`x'
	rename v_297_w`x' q6_9_B90_w`x'
	rename v_298_w`x' q6_9_FDP_w`x'
	rename v_299_w`x' q6_9_LIN_w`x'
	rename v_300_w`x' q6_9_AFD_w`x'
	rename v_301_w`x' q6_10_CDU_w`x'
	rename v_302_w`x' q6_10_CSU_w`x'
	rename v_303_w`x' q6_10_SPD_w`x'
	rename v_304_w`x' q6_10_B90_w`x'
	rename v_305_w`x' q6_10_FDP_w`x'
	rename v_306_w`x' q6_10_LIN_w`x'
	rename v_307_w`x' q6_10_AFD_w`x'
	rename v_308_w`x' q6_11_CDU_w`x'
	rename v_309_w`x' q6_11_CSU_w`x'
	rename v_310_w`x' q6_11_SPD_w`x'
	rename v_311_w`x' q6_11_B90_w`x'
	rename v_312_w`x' q6_11_FDP_w`x'
	rename v_313_w`x' q6_11_LIN_w`x'
	rename v_314_w`x' q6_11_AFD_w`x'
	rename v_315_w`x' q6_12_CDU_w`x'
	rename v_316_w`x' q6_12_CSU_w`x'
	rename v_317_w`x' q6_12_SPD_w`x'
	rename v_318_w`x' q6_12_B90_w`x'
	rename v_319_w`x' q6_12_FDP_w`x'
	rename v_320_w`x' q6_12_LIN_w`x'
	rename v_321_w`x' q6_12_AFD_w`x'
	rename v_322_w`x' q6_13_CDU_w`x'
	rename v_323_w`x' q6_13_CSU_w`x'
	rename v_324_w`x' q6_13_SPD_w`x'
	rename v_325_w`x' q6_13_B90_w`x'
	rename v_326_w`x' q6_13_FDP_w`x'
	rename v_327_w`x' q6_13_LIN_w`x'
	rename v_328_w`x' q6_13_AFD_w`x'
	rename v_329_w`x' q6_14_CDU_w`x'
	rename v_330_w`x' q6_14_CSU_w`x'
	rename v_331_w`x' q6_14_SPD_w`x'
	rename v_332_w`x' q6_14_B90_w`x'
	rename v_333_w`x' q6_14_FDP_w`x'
	rename v_334_w`x' q6_14_LIN_w`x'
	rename v_335_w`x' q6_14_AFD_w`x'
	rename v_336_w`x' q6_15_CDU_w`x'
	rename v_337_w`x' q6_15_CSU_w`x'
	rename v_338_w`x' q6_15_SPD_w`x'
	rename v_339_w`x' q6_15_B90_w`x'
	rename v_340_w`x' q6_15_FDP_w`x'
	rename v_341_w`x' q6_15_LIN_w`x'
	rename v_342_w`x' q6_15_AFD_w`x'
	rename v_343_w`x' q6_16_CDU_w`x'
	rename v_344_w`x' q6_16_CSU_w`x'
	rename v_345_w`x' q6_16_SPD_w`x'
	rename v_346_w`x' q6_16_B90_w`x'
	rename v_347_w`x' q6_16_FDP_w`x'
	rename v_348_w`x' q6_16_LIN_w`x'
	rename v_349_w`x' q6_16_AFD_w`x'
}
	
rename v_423_w3 q24_w3
rename v_424_w3 q25_w3
rename v_430_w3 q26_CDU_w3
rename v_431_w3 q26_SPD_w3
rename v_432_w3 q26_B90_w3
rename v_433_w3 q26_FDP_w3
rename v_434_w3 q26_LIN_w3
rename v_435_w3 q26_AFD_w3
rename v_437_w3 q26_oth_w3
rename v_436_w3 q26_inv_w3
rename v_438_w3 q26_non_w3
rename v_439_w3 q26_dk_w3
rename v_445_w3 q27_CDU_w3
rename v_446_w3 q27_SPD_w3
rename v_447_w3 q27_B90_w3
rename v_448_w3 q27_FDP_w3
rename v_449_w3 q27_LIN_w3
rename v_450_w3 q27_AFD_w3
rename v_452_w3 q27_oth_w3
rename v_451_w3 q27_inv_w3
rename v_453_w3 q27_all_w3
rename v_454_w3 q27_dk_w3
rename v_455_w3 q28_w3
rename v_456_w3 q29_w3
rename v_457_w3 q30_w3
rename v_463_w3 q31_CDU_w3
rename v_464_w3 q31_SPD_w3
rename v_465_w3 q31_B90_w3
rename v_466_w3 q31_FDP_w3
rename v_467_w3 q31_LIN_w3
rename v_468_w3 q31_AFD_w3
rename v_494_w3 q32_SPD_w3
rename v_495_w3 q32_B90_w3
rename v_496_w3 q32_FDP_w3
rename v_497_w3 q32_LIN_w3
rename v_498_w3 q32_AFD_w3
rename v_499_w3 q32_CDUOPP_w3
rename v_500_w3 q32_dk_w3
rename v_470_w3 q33_w3
rename v_476_w3 q34_1_w3
rename v_477_w3 q34_2_w3
rename v_478_w3 q34_3_w3
rename v_479_w3 q34_4_w3
rename v_480_w3 q34_5_w3
rename v_481_w3 q34_6_w3
rename v_482_w3 q34_7_w3
rename v_483_w3 q34_8_w3
rename v_484_w3 q34_9_w3
rename v_485_w3 q34_10_w3
rename v_486_w3 q34_11_w3
rename v_487_w3 q34_12_w3
rename v_488_w3 q34_13_w3
rename v_489_w3 q34_14_w3
rename v_490_w3 q34_15_w3
rename v_491_w3 q34_16_w3
rename v_492_w3 q34_17_w3


replace q7_w2 = q7_w1 if q7_w1==7


replace q8_w2 = q8_w1 if q8_w1!=-77 & q8_w1!=.

replace q9_CDU_w2 = q9_CDU_w1 if q9_CDU_w1!=-77 &  q9_CDU_w1 !=.
replace q9_SPD_w2 = q9_SPD_w1 if q9_SPD_w1!=-77 &  q9_SPD_w1 !=.
replace q9_B90_w2 = q9_B90_w1 if q9_B90_w1!=-77 &  q9_B90_w1 !=.
replace q9_FDP_w2 = q9_FDP_w1 if q9_FDP_w1!=-77 &  q9_FDP_w1 !=.
replace q9_LIN_w2 = q9_LIN_w1 if q9_LIN_w1!=-77 &  q9_LIN_w1 !=.
replace q9_AFD_w2 = q9_AFD_w1 if q9_AFD_w1!=-77 &  q9_AFD_w1 !=.
replace q9_oth_w2 = q9_oth_w1 if q9_oth_w1!=-77 &  q9_oth_w1 !=. 
replace q9_inv_w2 = q9_inv_w1 if q9_inv_w1!=-77 &  q9_inv_w1 !=.
replace q9_non_w2 = q9_non_w1 if q9_non_w1!=-77 &  q9_non_w1 !=.
replace q9_dk_w2  = q9_dk_w1  if q9_dk_w1 !=-77 &  q9_dk_w1  !=.

replace q10_CDU_w2 = q10_CDU_w1 if q10_CDU_w1!=-77 &  q10_CDU_w1 !=.
replace q10_SPD_w2 = q10_SPD_w1 if q10_SPD_w1!=-77 &  q10_SPD_w1 !=.
replace q10_B90_w2 = q10_B90_w1 if q10_B90_w1!=-77 &  q10_B90_w1 !=.
replace q10_FDP_w2 = q10_FDP_w1 if q10_FDP_w1!=-77 &  q10_FDP_w1 !=.
replace q10_LIN_w2 = q10_LIN_w1 if q10_LIN_w1!=-77 &  q10_LIN_w1 !=.
replace q10_AFD_w2 = q10_AFD_w1 if q10_AFD_w1!=-77 &  q10_AFD_w1 !=.
replace q10_oth_w2 = q10_oth_w1 if q10_oth_w1!=-77 &  q10_oth_w1 !=. 
replace q10_inv_w2 = q10_inv_w1 if q10_inv_w1!=-77 &  q10_inv_w1 !=.
replace q10_all_w2 = q10_all_w1 if q10_all_w1!=-77 &  q10_all_w1 !=.
replace q10_dk_w2  = q10_dk_w1  if q10_dk_w1 !=-77 &  q10_dk_w1  !=.

gen voted = 1 if  q24_w3==1

replace voted=0 if q24_w3!=1 & q24_w3!=.
tab voted

gen party_affiliation =  q25_w3
*replace party_affiliation = q8_w2 if party_affiliation==-77|party_affiliation==9|party_affiliation==.
tab party_affiliation
recode party (9=.) (8=.) (-77=.)
label def PARTY 1"CDU" 2"SPD" 3"GREEN" 4"FDP" 5"LINK" 6"AFD" 7"Andere" 
label val party PARTY

gen AFD=1 if party==6
replace AFD=0 if party!=6 & party!=. & party!=-77
tab AFD

gen LEFT=1 if party==5
replace LEFT= 0 if party!=5 & party!=. & party!=-77
tab LEFT


gen CDU=1 if party==1
replace CDU=0 if party!=1 & party!=. & party!=-77
tab CDU

gen SPD=1 if party==2
replace SPD= 0 if party!=2 & party!=. & party!=-77
tab SPD


gen GREEN=1 if party==3
replace GREEN=0 if party!=3 & party!=. & party!=-77
tab GREEN

gen FDP=1 if party==4
replace FDP= 0 if party!=4 & party!=. & party!=-77
tab FDP



replace child_in_hh=0 if children!=. & child_in_hh==.
replace num_child_hh= 0 if children!=. & num_child==. 
replace num_child_hh= -1 if num_child_hh==. 
replace child_in_hh= -1 if child_in_hh==. 
replace children= -1 if children==. 

gen undecided_w1 = 1 if q11_w1==9
replace undecided_w1= 0 if q11_w1>=1&q11_w1<=7

gen undecided_w2 = 1 if q11_w2==9
replace undecided_w2= 0 if q11_w2>=1&q11_w2<=7

saveold  "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_all_waves.dta" , replace  version(12)

**wave 1 data
clear all
use "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_all_waves.dta", replace
drop *w3 *w2
saveold "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_w1.dta", replace  version(12)

**wave 2 data
clear all
use "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_all_waves.dta", replace
drop *w3 *w1
saveold "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_w2.dta", replace  version(12)

**wave 3 data
clear all
use "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_all_waves.dta", replace
drop *w2 *w1
saveold "\\nas.uni-mannheim.de\uni-shares\swnsswml\Respondi\RESPONDI_w3\survey_daten\survey_data_w3.dta", replace  version(12)

