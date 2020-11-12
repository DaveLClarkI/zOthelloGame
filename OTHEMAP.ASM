OTHE    TITLE  'O T H E M A P   -  M A P S E T   F O R   O T H E P G M' 00000100
        PRINT  ON,GEN                                                   00000200
OTHEMAP DFHMSD TYPE=(&SYSPARM),MODE=INOUT,TIOAPFX=YES,TERM=3270-2,     X00000300
               CTRL=(FREEKB,FRSET),DATA=FIELD,LANG=COBOL,STORAGE=AUTO   00000400
        EJECT                                                           00000500
DATAMAP DFHMDI SIZE=(24,80),                                           X00000600
               JUSTIFY=LEFT,                                           X00000700
               LINE=NEXT,                                              X00000800
               COLUMN=SAME                                              00000900
        PRINT  ON,NOGEN                                                 00001000
        DFHMDF POS=(01,01),LENGTH=01,ATTRB=(ASKIP,NORM),INITIAL=' '     00001100
        DFHMDF POS=(01,11),LENGTH=15,ATTRB=(ASKIP,BRT),                X00001200
               INITIAL='O T H E L L O :'                                00001300
        DFHMDF POS=(01,29),LENGTH=03,ATTRB=(ASKIP,NORM),               X00001400
               INITIAL='( A'                                            00001500
        DFHMDF POS=(01,35),LENGTH=05,ATTRB=(ASKIP,BRT),                X00001600
               INITIAL='T W O'                                          00001700
        DFHMDF POS=(01,43),LENGTH=27,ATTRB=(ASKIP,NORM),               X00001800
               INITIAL='T E R M I N A L   G A M E )'                    00001900
        DFHMDF POS=(02,01),LENGTH=07,ATTRB=(ASKIP,BRT),                X00002000
               INITIAL='Author:'                                        00002100
        DFHMDF POS=(02,09),LENGTH=39,ATTRB=(ASKIP,NORM),               X00002200
               INITIAL='Dave Leonard Clark I                   '        00002300
        DFHMDF POS=(02,49),LENGTH=13,ATTRB=(ASKIP,BRT),                X00002400
               INITIAL='Installation:'                                  00002500
        DFHMDF POS=(02,63),LENGTH=17,ATTRB=(ASKIP,NORM),               X00002600
               INITIAL='Clarke Industries'                              00002700
        DFHMDF POS=(03,01),LENGTH=01,ATTRB=(ASKIP,NORM),INITIAL=' '     00002800
        DFHMDF POS=(04,01),LENGTH=01,ATTRB=(ASKIP,NORM),INITIAL=' '     00002900
TITTLE1 DFHMDF POS=(05,01),LENGTH=11,ATTRB=(ASKIP,NORM,FSET),          X00003000
               INITIAL='Player "X":'                                    00003100
PLAYER1 DFHMDF POS=(05,13),LENGTH=10,ATTRB=(ASKIP,BRT,FSET),           X00003200
               INITIAL='__________'                                     00003300
        DFHMDF POS=(05,24),LENGTH=01,ATTRB=(ASKIP,NORM),INITIAL=' '     00003400
PLAYER2 DFHMDF POS=(05,58),LENGTH=10,ATTRB=(ASKIP,BRT,FSET),           X00003500
               INITIAL='__________'                                     00003600
TITTLE2 DFHMDF POS=(05,69),LENGTH=11,ATTRB=(ASKIP,NORM,FSET),          X00003700
               INITIAL=':"O" Player'                                    00003800
        DFHMDF POS=(06,01),LENGTH=17,ATTRB=(ASKIP,NORM),               X00003900
               INITIAL='Positions you own'                              00004000
PIECES1 DFHMDF POS=(06,19),LENGTH=02,ATTRB=(ASKIP,BRT,FSET),           X00004100
               INITIAL='00',PICIN='99',PICOUT='99'                      00004200
        DFHMDF POS=(06,31),LENGTH=19,ATTRB=(ASKIP,NORM),               X00004300
               INITIAL='  A B C D E F G H  '                            00004400
PIECES2 DFHMDF POS=(06,60),LENGTH=02,ATTRB=(ASKIP,BRT,FSET),           X00004500
               INITIAL='00',PICIN='99',PICOUT='99'                      00004600
        DFHMDF POS=(06,63),LENGTH=17,ATTRB=(ASKIP,NORM),               X00004700
               INITIAL='Positions he owns'                              00004800
        DFHMDF POS=(07,01),LENGTH=12,ATTRB=(ASKIP,NORM),               X00004900
               INITIAL='Your Move is'                                   00005000
MOVEIS1 DFHMDF POS=(07,14),LENGTH=02,ATTRB=(UNPROT,BRT,FSET,IC),       X00005100
               INITIAL='__'                                             00005200
        DFHMDF POS=(07,17),LENGTH=01,ATTRB=(ASKIP,NORM),INITIAL=' '     00005300
        DFHMDF POS=(07,31),LENGTH=19,ATTRB=(ASKIP,BRT),                X00005400
               INITIAL='-------------------'                            00005500
MOVEIS2 DFHMDF POS=(07,65),LENGTH=02,ATTRB=(ASKIP,BRT,FSET),           X00005600
               INITIAL='__'                                             00005700
        DFHMDF POS=(07,68),LENGTH=12,ATTRB=(ASKIP,NORM),               X00005800
               INITIAL='was His Move'                                   00005900
        DFHMDF POS=(08,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00006000
               INITIAL='1'                                              00006100
        DFHMDF POS=(08,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00006200
               INITIAL=']'                                              00006300
ROW1    DFHMDF POS=(08,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00006400
               INITIAL='.',OCCURS=8                                     00006500
        DFHMDF POS=(08,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00006600
               INITIAL=']'                                              00006700
        DFHMDF POS=(08,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00006800
               INITIAL='1'                                              00006900
        DFHMDF POS=(09,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00007000
               INITIAL='2'                                              00007100
        DFHMDF POS=(09,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00007200
               INITIAL=']'                                              00007300
ROW2    DFHMDF POS=(09,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00007400
               INITIAL='.',OCCURS=8                                     00007500
        DFHMDF POS=(09,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00007600
               INITIAL=']'                                              00007700
        DFHMDF POS=(09,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00007800
               INITIAL='2'                                              00007900
        DFHMDF POS=(10,01),LENGTH=07,ATTRB=(ASKIP,BRT),                X00008000
               INITIAL='OBJECT:'                                        00008100
        DFHMDF POS=(10,09),LENGTH=12,ATTRB=(ASKIP,NORM),               X00008200
               INITIAL='To have  the'                                   00008300
        DFHMDF POS=(10,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00008400
               INITIAL='3'                                              00008500
        DFHMDF POS=(10,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00008600
               INITIAL=']'                                              00008700
ROW3    DFHMDF POS=(10,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00008800
               INITIAL='.',OCCURS=8                                     00008900
        DFHMDF POS=(10,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00009000
               INITIAL=']'                                              00009100
        DFHMDF POS=(10,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00009200
               INITIAL='3'                                              00009300
        DFHMDF POS=(10,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00009400
               INITIAL='pass by your turn.  '                           00009500
        DFHMDF POS=(11,01),LENGTH=20,ATTRB=(ASKIP,NORM),               X00009600
               INITIAL='most number of spots'                           00009700
        DFHMDF POS=(11,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00009800
               INITIAL='4'                                              00009900
        DFHMDF POS=(11,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00010000
               INITIAL=']'                                              00010100
ROW4    DFHMDF POS=(11,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00010200
               INITIAL='.',OCCURS=8                                     00010300
        DFHMDF POS=(11,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00010400
               INITIAL=']'                                              00010500
        DFHMDF POS=(11,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00010600
               INITIAL='4'                                              00010700
        DFHMDF POS=(11,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00010800
               INITIAL='    A valid  move is'                           00010900
        DFHMDF POS=(12,01),LENGTH=20,ATTRB=(ASKIP,NORM),               X00011000
               INITIAL='on the playing board'                           00011100
        DFHMDF POS=(12,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00011200
               INITIAL='5'                                              00011300
        DFHMDF POS=(12,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00011400
               INITIAL=']'                                              00011500
ROW5    DFHMDF POS=(12,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00011600
               INITIAL='.',OCCURS=8                                     00011700
        DFHMDF POS=(12,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00011800
               INITIAL=']'                                              00011900
        DFHMDF POS=(12,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00012000
               INITIAL='5'                                              00012100
        DFHMDF POS=(12,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00012200
               INITIAL='defined as positions'                           00012300
        DFHMDF POS=(13,01),LENGTH=20,ATTRB=(ASKIP,NORM),               X00012400
               INITIAL='occupied at the  end'                           00012500
        DFHMDF POS=(13,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00012600
               INITIAL='6'                                              00012700
        DFHMDF POS=(13,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00012800
               INITIAL=']'                                              00012900
ROW6    DFHMDF POS=(13,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00013000
               INITIAL='.',OCCURS=8                                     00013100
        DFHMDF POS=(13,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00013200
               INITIAL=']'                                              00013300
        DFHMDF POS=(13,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00013400
               INITIAL='6'                                              00013500
        DFHMDF POS=(13,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00013600
               INITIAL='on the playing board'                           00013700
        DFHMDF POS=(14,01),LENGTH=20,ATTRB=(ASKIP,NORM),               X00013800
               INITIAL='of the game.        '                           00013900
        DFHMDF POS=(14,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00014000
               INITIAL='7'                                              00014100
        DFHMDF POS=(14,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00014200
               INITIAL=']'                                              00014300
ROW7    DFHMDF POS=(14,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00014400
               INITIAL='.',OCCURS=8                                     00014500
        DFHMDF POS=(14,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00014600
               INITIAL=']'                                              00014700
        DFHMDF POS=(14,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00014800
               INITIAL='7'                                              00014900
        DFHMDF POS=(14,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00015000
               INITIAL='adjacent  to one  of'                           00015100
        DFHMDF POS=(15,01),LENGTH=20,ATTRB=(ASKIP,NORM),               X00015200
               INITIAL='    The game is over'                           00015300
        DFHMDF POS=(15,29),LENGTH=01,ATTRB=(ASKIP,NORM),               X00015400
               INITIAL='8'                                              00015500
        DFHMDF POS=(15,31),LENGTH=01,ATTRB=(ASKIP,BRT),                X00015600
               INITIAL=']'                                              00015700
ROW8    DFHMDF POS=(15,33),LENGTH=01,ATTRB=(ASKIP,NORM,FSET),          X00015800
               INITIAL='.',OCCURS=8                                     00015900
        DFHMDF POS=(15,49),LENGTH=01,ATTRB=(ASKIP,BRT),                X00016000
               INITIAL=']'                                              00016100
        DFHMDF POS=(15,51),LENGTH=01,ATTRB=(ASKIP,NORM),               X00016200
               INITIAL='8'                                              00016300
        DFHMDF POS=(15,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00016400
               INITIAL='your  opponents men,'                           00016500
        DFHMDF POS=(16,01),LENGTH=20,ATTRB=(ASKIP,NORM),               X00016600
               INITIAL='when  all  positions'                           00016700
        DFHMDF POS=(16,31),LENGTH=19,ATTRB=(ASKIP,BRT),                X00016800
               INITIAL='-------------------'                            00016900
        DFHMDF POS=(16,60),LENGTH=20,ATTRB=(ASKIP,NORM),               X00017000
               INITIAL='with one of your men'                           00017100
        DFHMDF POS=(17,01),LENGTH=21,ATTRB=(ASKIP,NORM),               X00017200
               INITIAL='on the playing  board'                          00017300
        DFHMDF POS=(17,31),LENGTH=19,ATTRB=(ASKIP,NORM),               X00017400
               INITIAL='  A B C D E F G H  '                            00017500
        DFHMDF POS=(17,59),LENGTH=21,ATTRB=(ASKIP,NORM),               X00017600
               INITIAL='beyond his man/men in'                          00017700
        DFHMDF POS=(18,01),LENGTH=23,ATTRB=(ASKIP,NORM),               X00017800
               INITIAL='are occupied or neither'                        00017900
        DFHMDF POS=(18,57),LENGTH=23,ATTRB=(ASKIP,NORM),               X00018000
               INITIAL='a direct line,  so long'                        00018100
        DFHMDF POS=(19,01),LENGTH=25,ATTRB=(ASKIP,NORM),               X00018200
               INITIAL='player  has a valid  move'                      00018300
        DFHMDF POS=(19,55),LENGTH=25,ATTRB=(ASKIP,NORM),               X00018400
               INITIAL='as no untaken intervening'                      00018500
        DFHMDF POS=(20,01),LENGTH=28,ATTRB=(ASKIP,NORM),               X00018600
               INITIAL='to make to unoccupied spots.'                   00018700
        DFHMDF POS=(20,52),LENGTH=28,ATTRB=(ASKIP,NORM),               X00018800
               INITIAL='positions are  between  your'                   00018900
        DFHMDF POS=(21,01),LENGTH=05,ATTRB=(ASKIP,BRT),                X00019000
               INITIAL='PLAY:'                                          00019100
        DFHMDF POS=(21,07),LENGTH=32,ATTRB=(ASKIP,NORM),               X00019200
               INITIAL='Alternating  turns are  taken by'               00019300
        DFHMDF POS=(21,42),LENGTH=38,ATTRB=(ASKIP,NORM),               X00019400
               INITIAL='man and the proposed position.        '         00019500
        DFHMDF POS=(22,01),LENGTH=38,ATTRB=(ASKIP,NORM),               X00019600
               INITIAL='each player by either  making a  valid'         00019700
        DFHMDF POS=(22,42),LENGTH=08,ATTRB=(ASKIP,BRT),                X00019800
               INITIAL='CAPTURE:'                                       00019900
        DFHMDF POS=(22,51),LENGTH=29,ATTRB=(ASKIP,NORM),               X00020000
               INITIAL='You  capture  your  opponents'                  00020100
        DFHMDF POS=(23,01),LENGTH=38,ATTRB=(ASKIP,NORM),               X00020200
               INITIAL='move or (if no valid moves are present'         00020300
        DFHMDF POS=(23,42),LENGTH=38,ATTRB=(ASKIP,NORM),               X00020400
               INITIAL='positions in all valid directions when'         00020500
        DFHMDF POS=(24,01),LENGTH=38,ATTRB=(ASKIP,NORM),               X00020600
               INITIAL='on the board) by pressing  "ENTER"  to'         00020700
        DFHMDF POS=(24,42),LENGTH=25,ATTRB=(ASKIP,NORM),               X00020800
               INITIAL='you make any valid move. '                      00020900
        DFHMDF POS=(24,68),LENGTH=12,ATTRB=(ASKIP,BRT),                X00021000
               INITIAL='GOOD LUCK!!!'                                   00021100
        PRINT  ON,GEN                                                   00021200
        EJECT                                                           00021300
        DFHMSD TYPE=FINAL                                               00021400
