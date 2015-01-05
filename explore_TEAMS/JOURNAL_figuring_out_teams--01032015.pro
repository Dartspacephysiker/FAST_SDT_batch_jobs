IDL Version 8.3 (linux x86_64 m64). (c) 2013, Exelis Visual Information Solutions, Inc.
Installation number: 212858-4.
Licensed for use by: Dartmouth College

Data dir set to /SPENCEdata2/Research/Cusp/database/
IDL> breakpoint,'/SPENCEdata2/software/sdt/batch_jobs/explore_TEAMS/explore_teams.pro',101
IDL> explore_teams
timespan is from 1997-01-23/00:00:01 to 1997-01-23/11:46:09
% Breakpoint at: EXPLORE_TEAMS     101 /SPENCEdata2/software/sdt/batch_jobs/exp
  lore_TEAMS/explore_teams.pro
IDL> retall
IDL> .run "/home/spencerh/software/sdt/idl/get_3dt.pro"
% Compiled module: GET_3DT.
IDL> .run "/SPENCEdata2/software/sdt/batch_jobs/explore_TEAMS/explore_teams.pro"
% Compiled module: EXPLORE_TEAMS.
IDL> explore_teams
timespan is from 1997-01-23/00:00:01 to 1997-01-23/11:46:09
% Breakpoint at: EXPLORE_TEAMS     101 /SPENCEdata2/software/sdt/batch_jobs/exp
  lore_TEAMS/explore_teams.pro
IDL> 
getDQISelection: Unable to get indices into DQI
No data extracted from STD buffers for 'Tms_HO_Survey_Packet_Hdr'
Error getting Header bytes for this packet.  Bytes will be nil.
getDQISelection: Unable to get indices into DQI
No data extracted from STD buffers for 'Tms_HO_Survey_Data'
 number of data points =      402
IDL> help,tmpj
** Structure <210dab8>, 3 tags, length=8056, data length=8056, refs=2:
   X               DOUBLE    Array[402]
   Y               FLOAT     Array[402, 3]
   YTITLE          STRING    'Jp'
IDL> get_2dt,'j_3d','fa_tsp',name='jp2d',t=t
% Compiled module: GET_2DT.
% Ambiguous keyword abbreviation: T.
% Execution halted at: EXPLORE_TEAMS     107
   /SPENCEdata2/software/sdt/batch_jobs/explore_TEAMS/explore_teams.pro
%                      $MAIN$          
IDL> get_2dt,'j_3d','fa_tsp',name='jp2d'
getDQISelection: Unable to get indices into DQI
No data extracted from STD buffers for 'Tms_HO_Survey_Packet_Hdr'
Error getting Header bytes for this packet.  Bytes will be nil.
getDQISelection: Unable to get indices into DQI
No data extracted from STD buffers for 'Tms_HO_Survey_Data'
% GET_2DT:       0.93900895 seconds execution time.
Number of data points =      578
IDL> retall
IDL> get_data,'jp2d',data=tmpj2d
IDL> print,tmpj2d.x(0)
   8.5401340e+08
IDL> for i=0,10 do print,time_to_str(tmpj2d.x(i)),time_to_str(tmpj.x(i))
% Object reference type required in this context: TMPJ.
% Execution halted at: $MAIN$          
IDL>   get_data,'Jp',data=tmpj

  get_data,'Jp',data=tmpj
IDL> 
IDL> for i=0,10 do print,time_to_str(tmpj2d.x(i))time_to_str(tmpj.x(i))
1997-01-23/09:56:361997-01-23/09:56:36
1997-01-23/09:56:401997-01-23/09:56:56
1997-01-23/09:56:521997-01-23/09:57:16
1997-01-23/09:56:561997-01-23/09:57:36
1997-01-23/09:57:001997-01-23/09:57:57
1997-01-23/09:57:121997-01-23/09:58:17
1997-01-23/09:57:161997-01-23/09:58:37
1997-01-23/09:57:201997-01-23/09:58:57
1997-01-23/09:57:321997-01-23/09:59:17
1997-01-23/09:57:361997-01-23/09:59:37
1997-01-23/09:57:401997-01-23/09:59:58
IDL> for i=0,10 do print,time_to_str(tmpj2d.x(i))+",    "+time_to_str(tmpj.x(i))
1997-01-23/09:56:36,    1997-01-23/09:56:36
1997-01-23/09:56:40,    1997-01-23/09:56:56
1997-01-23/09:56:52,    1997-01-23/09:57:16
1997-01-23/09:56:56,    1997-01-23/09:57:36
1997-01-23/09:57:00,    1997-01-23/09:57:57
1997-01-23/09:57:12,    1997-01-23/09:58:17
1997-01-23/09:57:16,    1997-01-23/09:58:37
1997-01-23/09:57:20,    1997-01-23/09:58:57
1997-01-23/09:57:32,    1997-01-23/09:59:17
1997-01-23/09:57:36,    1997-01-23/09:59:37
1997-01-23/09:57:40,    1997-01-23/09:59:58
IDL> print,t
   8.5401339e+08
IDL> print,time_to_str(t)
1997-01-23/09:56:26
IDL> help,tmpj
** Structure <210dab8>, 3 tags, length=8056, data length=8056, refs=2:
   X               DOUBLE    Array[402]
   Y               FLOAT     Array[402, 3]
   YTITLE          STRING    'Jp'
IDL> help,tmpj2d
** Structure <210d838>, 3 tags, length=11576, data length=11576, refs=2:
   X               DOUBLE    Array[578]
   Y               FLOAT     Array[578, 3]
   YTITLE          STRING    'jp2d'
IDL> for i=0,99 do print,str(tmpj2d.x(i+1)-tmpj2d.x(i))+",    "+str(tmpj.x(i+1)-tmpj.x(i))
% Compiled module: STR.
4.0000000,    20.105199
12.105199,    20.183594
4.0000000,    20.181875
4.0000000,    20.187178
12.183594,    20.180868
4.0000000,    20.187383
4.0000000,    20.185300
12.181875,    20.182147
4.0000000,    20.186294
4.0000000,    20.182266
12.187178,    20.184556
4.0000000,    20.186133
4.0000000,    20.184102
12.180868,    20.182225
4.0000000,    20.185676
4.0000000,    20.191219
12.187383,    20.185588
4.0000000,    11.357828
4.0000000,    2.5209732
12.185300,    2.5234375
4.0000000,    2.5258971
4.0000000,    2.5209484
12.182147,    2.5253906
4.0000000,    2.5239488
4.0000000,    2.5209731
12.186294,    2.5259019
4.0000000,    2.5253906
4.0000000,    2.5209486
12.182266,    2.5234374
4.0000000,    2.5234376
4.0000000,    2.5234374
12.184556,    2.5259020
4.0000000,    2.5234375
4.0000000,    2.5234340
12.186133,    2.5229076
4.0000000,    2.5224147
4.0000000,    2.5244603
12.184102,    2.5224147
4.0000000,    2.5239489
4.0000000,    2.5248765
12.182225,    2.5219812
4.0000000,    2.5234375
4.0000000,    2.5253907
12.185676,    2.5234375
4.0000000,    2.5253905
4.0000000,    2.5234375
12.191219,    2.5214599
4.0000000,    2.5234375
4.0000000,    2.5253906
12.185588,    2.5234375
4.0000000,    2.5250190
4.0000000,    2.5218560
7.3578279,    2.5248759
2.5209732,    2.5239326
2.5234375,    2.5248790
2.5258971,    2.5219960
2.5209484,    2.5234375
2.5253906,    2.5253905
2.5239488,    2.5229204
2.5209731,    2.5239203
2.5259019,    2.5253905
2.5253906,    2.5259023
2.5209486,    2.5209727
2.5234374,    2.5259023
2.5234376,    2.5209727
2.5234374,    2.5253686
2.5259020,    2.5234375
2.5234375,    2.5234375
2.5234340,    2.5253905
2.5229076,    2.5253907
2.5224147,    2.5214843
2.5244603,    2.5258981
2.5224147,    2.5229032
2.5239489,    2.5253906
2.5248765,    2.5259024
2.5219812,    2.5259024
2.5234375,    2.5229256
2.5253907,    2.5234342
2.5234375,    2.5258839
2.5253905,    2.5234375
2.5234375,    2.5199487
2.5214599,    2.5264143
2.5234375,    2.5229257
2.5253906,    2.5283675
2.5234375,    2.5229011
2.5250190,    2.5259025
2.5218560,    2.5229256
2.5248759,    2.5259026
2.5239326,    2.5234375
2.5248790,    2.5204604
2.5219960,    2.5278515
2.5234375,    2.5209497
2.5253905,    2.5259026
2.5229204,    2.5253905
2.5239203,    2.5259027
2.5253905,    2.5253906
2.5259023,    2.5234340
2.5209727,    2.5253721
2.5259023,    2.5204601
2.5209727,    2.5283680
IDL> for i=0,99 do print,time_to_str(tmpj2d.x(i))+",    "+time_to_str(tmpj.x(i))
1997-01-23/09:56:36,    1997-01-23/09:56:36
1997-01-23/09:56:40,    1997-01-23/09:56:56
1997-01-23/09:56:52,    1997-01-23/09:57:16
1997-01-23/09:56:56,    1997-01-23/09:57:36
1997-01-23/09:57:00,    1997-01-23/09:57:57
1997-01-23/09:57:12,    1997-01-23/09:58:17
1997-01-23/09:57:16,    1997-01-23/09:58:37
1997-01-23/09:57:20,    1997-01-23/09:58:57
1997-01-23/09:57:32,    1997-01-23/09:59:17
1997-01-23/09:57:36,    1997-01-23/09:59:37
1997-01-23/09:57:40,    1997-01-23/09:59:58
1997-01-23/09:57:53,    1997-01-23/10:00:18
1997-01-23/09:57:57,    1997-01-23/10:00:38
1997-01-23/09:58:01,    1997-01-23/10:00:58
1997-01-23/09:58:13,    1997-01-23/10:01:18
1997-01-23/09:58:17,    1997-01-23/10:01:39
1997-01-23/09:58:21,    1997-01-23/10:01:59
1997-01-23/09:58:33,    1997-01-23/10:02:19
1997-01-23/09:58:37,    1997-01-23/10:02:30
1997-01-23/09:58:41,    1997-01-23/10:02:33
1997-01-23/09:58:53,    1997-01-23/10:02:35
1997-01-23/09:58:57,    1997-01-23/10:02:38
1997-01-23/09:59:01,    1997-01-23/10:02:40
1997-01-23/09:59:13,    1997-01-23/10:02:43
1997-01-23/09:59:17,    1997-01-23/10:02:45
1997-01-23/09:59:21,    1997-01-23/10:02:48
1997-01-23/09:59:33,    1997-01-23/10:02:50
1997-01-23/09:59:37,    1997-01-23/10:02:53
1997-01-23/09:59:41,    1997-01-23/10:02:56
1997-01-23/09:59:54,    1997-01-23/10:02:58
1997-01-23/09:59:58,    1997-01-23/10:03:01
1997-01-23/10:00:02,    1997-01-23/10:03:03
1997-01-23/10:00:14,    1997-01-23/10:03:06
1997-01-23/10:00:18,    1997-01-23/10:03:08
1997-01-23/10:00:22,    1997-01-23/10:03:11
1997-01-23/10:00:34,    1997-01-23/10:03:13
1997-01-23/10:00:38,    1997-01-23/10:03:16
1997-01-23/10:00:42,    1997-01-23/10:03:18
1997-01-23/10:00:54,    1997-01-23/10:03:21
1997-01-23/10:00:58,    1997-01-23/10:03:23
1997-01-23/10:01:02,    1997-01-23/10:03:26
1997-01-23/10:01:14,    1997-01-23/10:03:28
1997-01-23/10:01:18,    1997-01-23/10:03:31
1997-01-23/10:01:22,    1997-01-23/10:03:33
1997-01-23/10:01:35,    1997-01-23/10:03:36
1997-01-23/10:01:39,    1997-01-23/10:03:38
1997-01-23/10:01:43,    1997-01-23/10:03:41
1997-01-23/10:01:55,    1997-01-23/10:03:43
1997-01-23/10:01:59,    1997-01-23/10:03:46
1997-01-23/10:02:03,    1997-01-23/10:03:49
1997-01-23/10:02:15,    1997-01-23/10:03:51
1997-01-23/10:02:19,    1997-01-23/10:03:54
1997-01-23/10:02:23,    1997-01-23/10:03:56
1997-01-23/10:02:30,    1997-01-23/10:03:59
1997-01-23/10:02:33,    1997-01-23/10:04:01
1997-01-23/10:02:35,    1997-01-23/10:04:04
1997-01-23/10:02:38,    1997-01-23/10:04:06
1997-01-23/10:02:40,    1997-01-23/10:04:09
1997-01-23/10:02:43,    1997-01-23/10:04:11
1997-01-23/10:02:45,    1997-01-23/10:04:14
1997-01-23/10:02:48,    1997-01-23/10:04:16
1997-01-23/10:02:50,    1997-01-23/10:04:19
1997-01-23/10:02:53,    1997-01-23/10:04:21
1997-01-23/10:02:56,    1997-01-23/10:04:24
1997-01-23/10:02:58,    1997-01-23/10:04:26
1997-01-23/10:03:01,    1997-01-23/10:04:29
1997-01-23/10:03:03,    1997-01-23/10:04:31
1997-01-23/10:03:06,    1997-01-23/10:04:34
1997-01-23/10:03:08,    1997-01-23/10:04:36
1997-01-23/10:03:11,    1997-01-23/10:04:39
1997-01-23/10:03:13,    1997-01-23/10:04:42
1997-01-23/10:03:16,    1997-01-23/10:04:44
1997-01-23/10:03:18,    1997-01-23/10:04:47
1997-01-23/10:03:21,    1997-01-23/10:04:49
1997-01-23/10:03:23,    1997-01-23/10:04:52
1997-01-23/10:03:26,    1997-01-23/10:04:54
1997-01-23/10:03:28,    1997-01-23/10:04:57
1997-01-23/10:03:31,    1997-01-23/10:04:59
1997-01-23/10:03:33,    1997-01-23/10:05:02
1997-01-23/10:03:36,    1997-01-23/10:05:04
1997-01-23/10:03:38,    1997-01-23/10:05:07
1997-01-23/10:03:41,    1997-01-23/10:05:09
1997-01-23/10:03:43,    1997-01-23/10:05:12
1997-01-23/10:03:46,    1997-01-23/10:05:14
1997-01-23/10:03:49,    1997-01-23/10:05:17
1997-01-23/10:03:51,    1997-01-23/10:05:19
1997-01-23/10:03:54,    1997-01-23/10:05:22
1997-01-23/10:03:56,    1997-01-23/10:05:24
1997-01-23/10:03:59,    1997-01-23/10:05:27
1997-01-23/10:04:01,    1997-01-23/10:05:29
1997-01-23/10:04:04,    1997-01-23/10:05:32
1997-01-23/10:04:06,    1997-01-23/10:05:35
1997-01-23/10:04:09,    1997-01-23/10:05:37
1997-01-23/10:04:11,    1997-01-23/10:05:40
1997-01-23/10:04:14,    1997-01-23/10:05:42
1997-01-23/10:04:16,    1997-01-23/10:05:45
1997-01-23/10:04:19,    1997-01-23/10:05:47
1997-01-23/10:04:21,    1997-01-23/10:05:50
1997-01-23/10:04:24,    1997-01-23/10:05:52
1997-01-23/10:04:26,    1997-01-23/10:05:55
IDL>   keep=where(finite(tmpj.y) NE 0)
  tx=tmpj.x(keep)
  ty=tmpj.y(keep)
  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(keep)) + " by removing junk data"

  keep=where(finite(tmpj.y) NE 0)
IDL>   tx=tmpj.x(keep)
IDL>   ty=tmpj.y(keep)
IDL>   print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
IDL>         + strcompress(n_elements(keep)) + " by removing junk data"
Original Jp data has  402 elements, but we're losing  1206 by removing junk data
IDL> 
IDL> help, keep
KEEP            LONG      = Array[1206]
IDL> help,tmpj.y
<Expression>    FLOAT     = Array[402, 3]
IDL>   keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
  tx=tmpj.x(keep)
  ty=[tmpj.y(keep,0),tmpj.y(keep,1),tmpj.y(keep,2)]

  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
IDL>   tx=tmpj.x(keep)
IDL>   ty=[tmpj.y(keep,0),tmpj.y(keep,1),tmpj.y(keep,2)]
IDL> 
IDL> help,ty
TY              FLOAT     = Array[1206]
IDL>   print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(keep)) + " by removing junk data"

  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
IDL>         + strcompress(n_elements(keep)) + " by removing junk data"
Original Jp data has  402 elements, but we're losing  402 by removing junk data
IDL> 
IDL>   print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"

  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
IDL>         + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
Original Jp data has  402 elements, but we're losing  0 by removing junk data
IDL> 
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              3 /tmp/idltemp25623mAG
IDL> help,ty
TY              FLOAT     = Array[1206]
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
Original Jp data has  402 elements, but we're losing  0 by removing junk data
% Illegal subscript range: TY.
% Execution halted at: $MAIN$             14 /tmp/idltemp25623mAG
IDL> help,keep
KEEP            LONG      = Array[1200]
IDL>   keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)

  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
IDL> 
IDL> help,keep
KEEP            LONG      = Array[402]
IDL>   tx=tmpj.x(keep)
  ty=[tmpj.y(keep,0),tmpj.y(keep,1),tmpj.y(keep,2)]

  tx=tmpj.x(keep)
IDL>   ty=[tmpj.y(keep,0),tmpj.y(keep,1),tmpj.y(keep,2)]
IDL> 
IDL>   keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
  tx=tmpj.x(keep)
  ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
  keep=where(abs(ty) GT 0.0)
  tx=tx(keep)
  ty=ty(keep)

  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
IDL>   tx=tmpj.x(keep)
IDL>   ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
IDL>   print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
IDL>         + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
Original Jp data has  402 elements, but we're losing  0 by removing junk data
IDL>   keep=where(abs(ty) GT 0.0)
IDL>   tx=tx(keep)
IDL>   ty=ty(keep)
IDL> 
IDL> help,ty
TY              FLOAT     = Array[1200]
IDL>   keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
  tx=tmpj.x(keep)
  ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]

  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
IDL>   tx=tmpj.x(keep)
IDL>   ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
IDL> 
IDL> help,ty
TY              FLOAT     = Array[402, 3]
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
Original Jp data has  402 elements, but we're losing  0 by removing junk data
IDL> help,ty
TY              FLOAT     = Array[400]
IDL>   ty(*,0)=ty(keep,0)
  ty(*,1)=ty(keep,1)
  ty(*,2)=ty(keep,2)

  ty(*,0)=ty(keep,0)
IDL>   ty(*,1)=ty(keep,1)
% Illegal subscript range: TY.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL>   ty(*,2)=ty(keep,2)
% Illegal subscript range: TY.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL> 
IDL>   get_data,'Jp',data=tmpj
  get_data,'jp2d',data=tmpj2d
  
  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
  tx=tmpj.x(keep)
  ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
  keep=where(abs(ty(*,0)) GT 0.0 AND abs(ty(*,1)) GT 0.0 AND abs(ty(*,2)) GT 0.0)
  tx=tx(keep)
;  ty=ty(keep)
  ty(*,0)=ty(keep,0)
  ty(*,1)=ty(keep,1)
  ty(*,2)=ty(keep,2)

  get_data,'Jp',data=tmpj
IDL>   get_data,'jp2d',data=tmpj2d
IDL>   
IDL>   keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
IDL>   tx=tmpj.x(keep)
IDL>   ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
IDL>   print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
IDL>         + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
Original Jp data has  402 elements, but we're losing  0 by removing junk data
IDL>   keep=where(abs(ty(*,0)) GT 0.0 AND abs(ty(*,1)) GT 0.0 AND abs(ty(*,2)) GT 0.0)
IDL>   tx=tx(keep)
IDL> ;  ty=ty(keep)
IDL>   ty(*,0)=ty(keep,0)
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL>   ty(*,1)=ty(keep,1)
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL>   ty(*,2)=ty(keep,2)
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL> 
IDL>   get_data,'Jp',data=tmpj
  get_data,'jp2d',data=tmpj2d
  
  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
  tx=tmpj.x(keep)
  ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
  keep=where(abs(ty(*,0)) GT 0.0 AND abs(ty(*,1)) GT 0.0 AND abs(ty(*,2)) GT 0.0)
  tx=tx(keep)
;  ty=ty(keep)
  ty(*,0)=ty(keep,0)
  ty(*,1)=ty(keep,1)
  ty(*,2)=ty(keep,2)
  

  get_data,'Jp',data=tmpj
IDL>   get_data,'jp2d',data=tmpj2d
IDL>   
IDL>   keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
IDL>   tx=tmpj.x(keep)
IDL>   ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
IDL>   print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
IDL>         + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
Original Jp data has  402 elements, but we're losing  0 by removing junk data
IDL>   keep=where(abs(ty(*,0)) GT 0.0 AND abs(ty(*,1)) GT 0.0 AND abs(ty(*,2)) GT 0.0)
IDL>   tx=tx(keep)
IDL> ;  ty=ty(keep)
IDL>   ty(*,0)=ty(keep,0)
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL>   ty(*,1)=ty(keep,1)
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL>   ty(*,2)=ty(keep,2)
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$              1 /tmp/idltemp25623mAG
IDL>   
IDL> 
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
Original Jp data has  402 elements, but we're losing  0 by removing junk data
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$             12 /tmp/idltemp25623mAG
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
Original Jp data has  402 elements, but we're losing  0 by removing junk data
% Array subscript for TY must have same size as source expression.
% Execution halted at: $MAIN$             13 /tmp/idltemp25623mAG
IDL> help,keep
KEEP            LONG      = Array[400]
IDL> help,ty
TY              FLOAT     = Array[402, 3]
IDL>   ty=ty(keep,*)

  ty=ty(keep,*)
IDL> 
IDL> help,ty
TY              FLOAT     = Array[400, 3]
IDL>   ;; get timescale monotonic
  time_order=sort(tx)
  tx=tx(time_order)
;  ty(*,0)=ty(time_order,0)
;  ty(*,1)=ty(time_order,1)
;  ty(*,2)=ty(time_order,2)
  ty=ty(time_order,*)

  ;; get timescale monotonic
IDL>   time_order=sort(tx)
IDL>   tx=tx(time_order)
IDL> ;  ty(*,0)=ty(time_order,0)
IDL> ;  ty(*,1)=ty(time_order,1)
IDL> ;  ty(*,2)=ty(time_order,2)
IDL>   ty=ty(time_order,*)
IDL> 
IDL> help,ty
TY              FLOAT     = Array[400, 3]
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
% Compiled module: AURORAL_ZONE.
IDL> help,ty
TY              FLOAT     = Array[400, 3]
IDL> help,keep
KEEP            LONG      = Array[356]
IDL> help,jp
** Structure <2277e18>, 2 tags, length=7800, data length=7800, refs=1:
   X               DOUBLE    Array[390]
   Y               FLOAT     Array[390, 3]
IDL> get_data,'Jp',data=jp
IDL> help,jp
** Structure <218e028>, 2 tags, length=7120, data length=7120, refs=2:
   X               DOUBLE    Array[356]
   Y               FLOAT     Array[356, 3]
IDL> .run "/tmp/idltemp25623mAG"
% Compiled module: $MAIN$.
Original Jp data has  356 elements, but we're losing  0 by removing junk data
number_of_intervals           2
IDL> help,time_ranges
TIME_RANGES     DOUBLE    = Array[2, 2]
IDL> print,time_ranges
   8.5401376e+08   8.5401514e+08
   8.5401414e+08   8.5401537e+08
IDL> print,time_to_str(time_ranges)
1997-01-23/10:02:38 1997-01-23/10:25:39 1997-01-23/10:08:58 1997-01-23/10:29:31
