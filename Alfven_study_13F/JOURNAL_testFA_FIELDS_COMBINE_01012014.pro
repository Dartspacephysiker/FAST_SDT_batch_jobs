IDL> CD, '/home/spencerh/Desktop'
IDL> @startup
ON_ERROR set to 1...
% Compiled module: LOADCT2.
% Unsupported X Windows visual (class: PseudoColor, depth: 8).
  Substituting default (class: TrueColor, Depth: 24).
% LOADCT: Loading table Rainbow + white
    16777216 colors available.
IDL> get_data,'MagDCcomp3',data=magz
% Compiled module: GET_DATA.
% Compiled module: FIND_HANDLE.
% FIND_HANDLE: No Data stored yet!
IDL> get_data,'Mag3DC',data=magz
% FIND_HANDLE: No Data stored yet!
IDL> get_data,'MagDC',data=magz
% FIND_HANDLE: No Data stored yet!
IDL> get_dqds()

get_dqds()
 ^
% Syntax error.
IDL> print,get_dqds()
% Compiled module: GET_DQDS.
% Compiled module: SHOW_DQIS.
% Compiled module: GET_SDT_RUN_IDX.
% Compiled module: DEFINED.
% Compiled module: DATETIMESEC.
% Compiled module: DATESEC.
% Compiled module: TIMESEC.
AttitudeCtrl DataHdr_1032 Eesa Burst Eesa Survey Eesa_Burst_Packet_Hdr Eesa_Survey_Packet_Hdr
Fast_Attitude_Data Fast_Orbit_Data Iesa Burst Iesa Survey Iesa_Burst_Packet_Hdr Iesa_Survey_Packet_Hdr
Mag1ac_S Mag1dc_S Mag2ac_S Mag2dc_S Mag3ac_S Mag3dc_S MagDC MagXYZ SMPhase_FieldsSurvey0 Sesa 1 Burst
Sesa 2 Burst Sesa 3 Burst Sesa 4 Burst Sesa 5 Burst Sesa 6 Burst Sesa Survey Sesa1_Burst_Packet_Hdr
Sesa1_Burst_SpinPhase_Info Sesa2_Burst_Packet_Hdr Sesa2_Burst_SpinPhase_Info Sesa3_Burst_Packet_Hdr
Sesa3_Burst_SpinPhase_Info Sesa4_Burst_Packet_Hdr Sesa4_Burst_SpinPhase_Info Sesa5_Burst_Packet_Hdr
Sesa5_Burst_SpinPhase_Info Sesa6_Burst_Packet_Hdr Sesa6_Burst_SpinPhase_Info Sesa_Survey_Packet_Hdr
Sesa_Survey_SpinPhase_Info Tms_HO_Survey_Data Tms_HO_Survey_Packet_Hdr Tms_He_Survey_Data
Tms_He_Survey_Packet_Hdr V1-V2_S V1-V4_S V5-V8_S V8_S
IDL> get_data,'MagDC',data=magdc
% FIND_HANDLE: No Data stored yet!
IDL> field=get_fa_fields('MagDC',/all,/store)
% Compiled module: GET_FA_FIELDS.
% Compiled module: IDL_TYPE.
% Compiled module: MISSING_DQDS.
% Compiled module: DETECT_2D_FIELDS.
% Compiled module: GET_TS_FROM_SDT.
% Compiled module: ALLOCATEARRAY.
% Compiled module: DATESEC_VAR.
% Compiled module: STORE_DATA.
% Compiled module: DATA_TYPE.
% Compiled module: EXTRACT_TAGS.
% Compiled module: DIMEN1.
% Compiled module: FIND_STR_ELEMENT.
% Compiled module: ADD_STR_ELEMENT.
% Compiled module: STR_ELEMENT.
% Compiled module: MINMAX_RANGE.
IDL> help,field
FIELD           STRING    = Array[3]
IDL> print field

print field
       ^
% Syntax error.
IDL> print,field
MagDCcomp1 MagDCcomp2 MagDCcomp3
IDL> get_data,'MagDCcomp3',data=magz
% Compiled module: NDIMEN.
IDL> store_data,'MagZ',data=magz
IDL> 			;magz.y=smooth(magz.y,40)
IDL> 			store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
IDL> efieldV1214=get_fa_fields('V1-V2_S',/all)
IDL> efieldV58=get_fa_fields('V5-V8_S',/all)
IDL> FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine
% Compiled module: FA_FIELDS_COMBINE.
IDL> efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}
