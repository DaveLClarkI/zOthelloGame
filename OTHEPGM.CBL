000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    OTHEPGM.
000300 AUTHOR.        DAVE L CLARK.
000400 INSTALLATION.  CLARKE INDUSTRIES.
000500 DATE-WRITTEN.  JUN 3, 1983.
000600 DATE-COMPILED.
000700 SECURITY.      CICS SECURITY KEY ONLY.
000800 REMARKS.       OTHELLO (TWO TERMINAL) PROGRAM.
000900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
001000*                                                               *
001100* CLARKE Industries, Inc.                       Written by:     *
001200* 2100 Highway 265                           Dave Leonard Clark *
001300* Springdale, AR  72764                        (501)750-8248    *
001400*                                                               *
001500* No warranty is expressed or implied.   Neither the author nor *
001600* the company is responsible  for  any loss or damage resulting *
001700* from the use of this  code.   Source  is provided on an as-is *
001800* basis.                                                        *
001900*                                                               *
002000* ============================================================= *
002100*                                                               *
002200* This program is invoked as a CICS transaction as follows:     *
002300*                                                               *
002400* OTHE tttt                                                     *
002500*                                                               *
002600* This is entered from a  blank  CICS  screen and "tttt" is the *
002700* terminal identification for your desired opponent.            *
002800*                                                               *
002900* Computerized opponents can be written for this game and named *
003000* the same as this program  with  a  suffix of "0" to "9" (e.g. *
003100* the suffix could represent the level of expertise).  To use a *
003200* computerized opponent,  start the game with your own terminal *
003300* identification specified then enter  your  name as one player *
003400* and the user-written program's name  (in all capitals) as the *
003500* other player (the first player  name entered is the first one *
003600* to move).   See the "INTERVAL-LINK"  storage area below for a *
003700* layout of the  passed  information.   Your program should not *
003800* modify  any  fields  in  the  communication  area  except the *
003900* "INTERVAL-PREVMV" field in which it should place its move.    *
004000*                                                               *
004100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
004200 ENVIRONMENT DIVISION.
004300 CONFIGURATION SECTION.
004400 SOURCE-COMPUTER. IBM-4381-T91E.
004500 OBJECT-COMPUTER. IBM-4381-T91E.
004600 DATA DIVISION.
004700 WORKING-STORAGE SECTION.
004800 77  FILLER                      PIC X(58)    VALUE IS
004900     "*****   OTHELLO GAME PROGRAM      - DAVE L. CLARK    *****".
005000*
005100*  THE FIELD BELOW IS ALL THE LOWER CASE ALPHABETIC CHARACTERS!
005200 77  LOWER-CASE      PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
005300*
005400*  THE FIELD BELOW IS ALL THE UPPER CASE ALPHABETIC CHARACTERS!
005500 77  UPPER-CASE      PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
005600*
005700 77  PIECE                       PIC X        VALUE SPACES.
005800 77  MOVER                       PIC 9        VALUE ZEROES.
005900 77  MOVEC                       PIC 9        VALUE ZEROES.
006000 01  CHECK-MOVE.
006100     05  MOVEBYT1                PIC X.
006200     05  MOVEBYT2                PIC X.
006300 01  PLAYER-DATA.
006400     05  FILLER        PIC X(22) VALUE "Player 'X'::'X' Player".
006500     05  FILLER        PIC X(22) VALUE "Player 'O'::'O' Player".
006600 01  PLAYER-TBLE REDEFINES PLAYER-DATA.
006700     05  FILLER OCCURS 2 TIMES.
006800         10  PLAYER OCCURS 2 TIMES PIC X(11).
006900 01  ERASEL                      PIC S9(4)    COMP VALUE +0056.
007000 01  ERASEO.
007100     05  ERASEO-SBA              PIC X        VALUE SPACES.
007200     05  ERASEO-BAD              PIC XX       VALUE SPACES.
007300     05  ERASEO-FIL              PIC X(53)    VALUE SPACES.
007400 01  MESSAGL                     PIC S9(4)    COMP VALUE +0057.
007500 01  MESSAGO.
007600     05  MESSAGO-SBA             PIC X        VALUE SPACES.
007700     05  MESSAGO-BAD             PIC XX       VALUE SPACES.
007800     05  MESSAGO-STF             PIC X        VALUE SPACES.
007900     05  MESSAGO-ATR             PIC X        VALUE SPACES.
008000     05  MESSAGO-MSG             PIC X(52)    VALUE IS
008100         "ENTER YOUR NAME IN THE SPACES PROVIDED AT THE CURSOR".
008200 01  I-O-LEN                     PIC S9(4)    COMP VALUE +1920.
008300 01  OUT-MSG.
008400     05  OUT-MES                 PIC X(46)    VALUE SPACES.
008500     05  OUT-RTC                 PIC X(4)     VALUE SPACES.
008600 01  INP-MSG.
008700     05  INP-TRAN                PIC X(4)     VALUE SPACES.
008800     05  FILLER                  PIC X        VALUE SPACES.
008900     05  INP-TERM                PIC X(4)     VALUE SPACES.
009000 01  TERM-IOA                    PIC X(2020)  VALUE LOW-VALUES.
009100 01  ACK-LENG                    PIC S9(4)    COMP VALUE +0079.
009200 01  ACK-MESG.
009300     05  FILLER                  PIC X(39)    VALUE IS
009400         "YOU HAVE NOW BEEN LINKED WITH TERMINAL ".
009500     05  ACK-TERM                PIC X(4).
009600     05  FILLER                  PIC X(36)    VALUE IS
009700         " - PLEASE PRESS ENTER AND STAND BY -".
009800 01  ENEMY                       PIC 9        VALUE ZERO.
009900 01  PLAYR                       PIC 9        VALUE ZERO.
010000 01  OTHELLO                     PIC X(4)     VALUE "OTHE".
010100 01  QUEUE-NAME                  PIC X(8)     VALUE "OTHELLO ".
010200 01  QUEUE-RLEN                  PIC S9(4)    COMP VALUE +0256.
010300 01  INTERVAL-TERM               PIC X(4)     VALUE SPACES.
010400 01  INTERVAL-LENG               PIC S9(4)    COMP VALUE +0286.
010500 01  INTERVAL-LINK.
010600     05  INTERVAL-PLAYNO         PIC 9        VALUE 1.
010700     05  INTERVAL-PREVMV         PIC XX       VALUE "SP".
010800     05  INTERVAL-MOVENO         PIC S9(4)    COMP VALUE ZEROES.
010900     05  INTERVAL-MVMODE         PIC X        VALUE SPACES.
011000     05  INTERVAL-PIECES         PIC 99       OCCURS 2 TIMES.
011100     05  INTERVAL-PNAMES         PIC X(10)    OCCURS 2 TIMES.
011200     05  INTERVAL-PBOARD         PIC X(256).
011300 01  WS-MOVE-CHECK-AREA.
011400     05  CURRENT-PLAYER          PIC X        VALUE SPACES.
011500     05  OPPOSING-PLAYER         PIC X        VALUE SPACES.
011600     05  MOVE-STATUS             PIC XX       VALUE SPACES.
011700         88  VALID-MOVE                       VALUE "OK".
011800         88  INVALID-MOVE                     VALUE "NO".
011900     05  DIRECTION               PIC 9        VALUE ZERO.
012000     05  FOUND-OPPONENT          PIC 9        VALUE ZERO.
012100     05  FLIP-CNT                PIC 99       VALUE ZERO.
012200     05  SUBR                    PIC 9        VALUE ZERO.
012300     05  SUBC                    PIC 9        VALUE ZERO.
012400     05  DIR-DATA.
012500         10  FILLER              PIC S9       VALUE -1.
012600         10  FILLER              PIC S9       VALUE +0.
012700         10  FILLER              PIC S9       VALUE -1.
012800         10  FILLER              PIC S9       VALUE +1.
012900         10  FILLER              PIC S9       VALUE +0.
013000         10  FILLER              PIC S9       VALUE +1.
013100         10  FILLER              PIC S9       VALUE +1.
013200         10  FILLER              PIC S9       VALUE +1.
013300         10  FILLER              PIC S9       VALUE +1.
013400         10  FILLER              PIC S9       VALUE +0.
013500         10  FILLER              PIC S9       VALUE +1.
013600         10  FILLER              PIC S9       VALUE -1.
013700         10  FILLER              PIC S9       VALUE +0.
013800         10  FILLER              PIC S9       VALUE -1.
013900         10  FILLER              PIC S9       VALUE -1.
014000         10  FILLER              PIC S9       VALUE -1.
014100     05  DIR-TBL REDEFINES DIR-DATA OCCURS 8 TIMES.
014200         10  DIR-TBLR            PIC S9.
014300         10  DIR-TBLC            PIC S9.
014400 01  WS-CONVRT.
014500     05  MULT-CONVRT             PIC 99       VALUE ZEROES.
014600     05  SUBBT-CONVRT            PIC 99       VALUE ZEROES.
014700     05  SUBHX-CONVRT            PIC S9(3)    VALUE ZEROES.
014800 01  AREA-D-CONVRT.
014900     05  DECHEX-CONVRT           PIC S9(4)    COMP   VALUE ZEROES.
015000 01  AREA-H-CONVRT REDEFINES     AREA-D-CONVRT.
015100     05  FILLER                  PIC X.
015200     05  HEXDEC-CONVRT           PIC X.
015300 01  AREA-I-CONVRT.
015400     05  BYTI-1-CONVRT           PIC X        OCCURS 80 TIMES.
015500     05  FILLER                  PIC X(80).
015600 01  FILLER        REDEFINES     AREA-I-CONVRT.
015700     05  BYTI-2-CONVRT           PIC XX       OCCURS 80 TIMES.
015800 01  AREA-O-CONVRT.
015900     05  BYTO-1-CONVRT           PIC X        OCCURS 80 TIMES.
016000     05  FILLER                  PIC X(80).
016100 01  FILLER        REDEFINES     AREA-O-CONVRT.
016200     05  BYTO-2-CONVRT           PIC XX       OCCURS 80 TIMES.
016300 01  DATA-HEX-CONVRT.
016400     05 FILLER PIC X(32) VALUE "000102030405060708090A0B0C0D0E0F".
016500     05 FILLER PIC X(32) VALUE "101112131415161718191A1B1C1D1E1F".
016600     05 FILLER PIC X(32) VALUE "202122232425262728292A2B2C2D2E2F".
016700     05 FILLER PIC X(32) VALUE "303132333435363738393A3B3C3D3E3F".
016800     05 FILLER PIC X(32) VALUE "404142434445464748494A4B4C4D4E4F".
016900     05 FILLER PIC X(32) VALUE "505152535455565758595A5B5C5D5E5F".
017000     05 FILLER PIC X(32) VALUE "606162636465666768696A6B6C6D6E6F".
017100     05 FILLER PIC X(32) VALUE "707172737475767778797A7B7C7D7E7F".
017200     05 FILLER PIC X(32) VALUE "808182838485868788898A8B8C8D8E8F".
017300     05 FILLER PIC X(32) VALUE "909192939495969798999A9B9C9D9E9F".
017400     05 FILLER PIC X(32) VALUE "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF".
017500     05 FILLER PIC X(32) VALUE "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF".
017600     05 FILLER PIC X(32) VALUE "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF".
017700     05 FILLER PIC X(32) VALUE "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF".
017800     05 FILLER PIC X(32) VALUE "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF".
017900     05 FILLER PIC X(32) VALUE "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF".
018000 01  TBLE-HEX-CONVRT REDEFINES DATA-HEX-CONVRT.
018100     05 HEX-VALUE-CONVRT         PIC XX
018200               OCCURS 256 TIMES, INDEXED BY TBLES-CONVRT.
018300 01  DATA-ADR-CONVRT.
018400     05  FILLER                  PIC X(9)     VALUE "268435456".
018500     05  FILLER                  PIC X(9)     VALUE "016777216".
018600     05  FILLER                  PIC X(9)     VALUE "001048576".
018700     05  FILLER                  PIC X(9)     VALUE "000065536".
018800     05  FILLER                  PIC X(9)     VALUE "000004096".
018900     05  FILLER                  PIC X(9)     VALUE "000000256".
019000     05  FILLER                  PIC X(9)     VALUE "000000016".
019100     05  FILLER                  PIC X(9)     VALUE "000000001".
019200 01  TBLE-ADR-CONVRT REDEFINES DATA-ADR-CONVRT.
019300     05 ADR-VALUE-CONVRT         PIC 9(9)     OCCURS 8 TIMES.
019400 01  USER-CONVRT.
019500     05  RQC-CONVRT              PIC 99       VALUE ZEROES.
019600         88  CHR-DEC-CONVRT                     VALUE 01.
019700         88  CHR-HEX-CONVRT                     VALUE 02.
019800         88  CHR-HEX-STR-CONVRT                 VALUE 03.
019900         88  HEX-DEC-CONVRT                     VALUE 04.
020000         88  HEX-CHR-CONVRT                     VALUE 05.
020100         88  HEX-CHR-STR-CONVRT                 VALUE 06.
020200         88  DEC-HEX-CONVRT                     VALUE 07.
020300         88  DEC-CHR-CONVRT                     VALUE 08.
020400         88  ADR-DEC-CONVRT                     VALUE 09.
020500         88  DEC-ADR-CONVRT                     VALUE 10.
020600     05  CHAR-CONVRT             PIC X        VALUE SPACES.
020700     05  HEX-CONVRT              PIC XX       VALUE SPACES.
020800     05  ADR-CONVRT              PIC X(8)     VALUE SPACES.
020900     05  DEC-CONVRT              PIC S9(11)   COMP-3 VALUE ZEROES.
021000     05  INP-CONVRT              PIC X(160)   VALUE SPACES.
021100     05  LEN-CONVRT              PIC 99       VALUE ZEROES.
021200     05  OUT-CONVRT              PIC X(160)   VALUE SPACES.
021300     05  RTC-CONVRT              PIC XX       VALUE SPACES.
021400         88  COMPLETED-CONVRT                   VALUE "00".
021500         88  INVA-REQU-CONVRT                   VALUE "01".
021600         88  INVA-CHAR-CONVRT                   VALUE "02".
021700         88  INVA-HEX-CONVRT                    VALUE "03".
021800         88  INVA-DEC-CONVRT                    VALUE "04".
021900         88  INVA-LENG-CONVRT                   VALUE "05".
022000******************************************************************
022100*                                                                *
022200*    DEFAULT TERMINAL I/O AREA                                   *
022300*                                                                *
022400******************************************************************
022500 01  OTHEMAP                     PIC X(8)     VALUE "OTHEMAP ".
022600 01  DATAMAP                     PIC X(8)     VALUE "DATAMAP ".
022700     COPY "OTHEMAP".
022800 01  DATAMAPX  REDEFINES  DATAMAPO.
022900     05  FILLER                  PIC X(86).
023000     05  PLAYING-BOARD.
023100         10  FILLER  OCCURS 8 TIMES.
023200             15  FILLER  OCCURS 8 TIMES.
023300                 20  POSXL       PIC S9(4)    COMP.
023400                 20  POSXA       PIC X.
023500                 20  POSXI       PIC X.
023600                 20  POSXO REDEFINES POSXI PIC X.
023700 01  SCRATTRB.
023800     02  SCRUNPRO                PIC X        VALUE " ".
023900*          X"40"= UNPRO NORM                    GREEN ON 3279 TERM
024000     02  SCRUNNUM                PIC X        VALUE "&".
024100*          X"50"= UNPRO NORM NUM                GREEN ON 3279 TERM
024200     02  SCRUNFST                PIC X        VALUE "A".
024300*          X"C1"= UNPRO NORM FSET               GREEN ON 3279 TERM
024400     02  SCRUNNMF                PIC X        VALUE "J".
024500*          X"D1"= UNPRO NORM NUM FSET           GREEN ON 3279 TERM
024600     02  SCRUNBRT                PIC X        VALUE "H".
024700*          X"C8"= UNPRO BRT                      RED  ON 3279 TERM
024800     02  SCRUNBRF                PIC X        VALUE "I".
024900*          X"C9"= UNPRO BRT  FSET                RED  ON 3279 TERM
025000     02  SCRUNBRN                PIC X        VALUE "Q".
025100*          X"D8"= UNPRO BRT  NUM                 RED  ON 3279 TERM
025200     02  SCRUNBNF                PIC X        VALUE "R".
025300*          X"D9"= UNPRO BRT  NUM FSET            RED  ON 3279 TERM
025400     02  SCRUNDRK                PIC X        VALUE "<".
025500*          X"4C"= UNPRO DRK                     BLANK ON  ANY TERM
025600     02  SCRUNDKF                PIC X        VALUE "(".
025700*          X"4D"= UNPRO DRK  FSET               BLANK ON  ANY TERM
025800     02  SCRUNDKN                PIC X        VALUE "*".
025900*          X"5C"= UNPRO DRK  NUM                BLANK ON  ANY TERM
026000     02  SCRUNDNF                PIC X        VALUE ")".
026100*          X"5D"= UNPRO DRK  NUM FSET           BLANK ON  ANY TERM
026200     02  SCRASKIP                PIC X        VALUE "0".
026300*          X"F0"= ASKIP NORM                    BLUE  ON 3279 TERM
026400     02  SCRASFST                PIC X        VALUE "1".
026500*          X"F1"= ASKIP NORM FSET               BLUE  ON 3279 TERM
026600     02  SCRASBRT                PIC X        VALUE "8".
026700*          X"F8"= ASKIP BRT                     WHITE ON 3279 TERM
026800     02  SCRASBRF                PIC X        VALUE "9".
026900*          X"F9"= ASKIP BRT  FSET               WHITE ON 3279 TERM
027000     02  SCRASDRK                PIC X        VALUE "@".
027100*          X"7C"= ASKIP DRK                     BLANK ON  ANY TERM
027200     02  SCRASDKF                PIC X        VALUE "'".
027300*          X"7D"= ASKIP DRK  FSET               BLANK ON  ANY TERM
027400     02  SCRSETBA                PIC X        VALUE "".
027500*          X"11"= SET SCREEN BUFFER ADDR (2-BYTE ADDR MUST FOLLOW
027600     02  SCRERASE                PIC X        VALUE "".
027700*          X"12"= ERASE UNPROT TO ADDR (2-BYTE ADDR MUST FOLLOW)
027800     02  SCRCURSR                PIC X        VALUE "".
027900*          X"13"= INSERT CURSOR
028000     02  SCRSTRTF                PIC X        VALUE "".
028100*          X"1D"= START SCREEN FIELD (1-BYTE ATTRB MUST FOLLOW)
028200     02  SCREPEAT                PIC X        VALUE "".
028300*          X"3C"= REPEAT TO ADDR (2-BYTE ADDR + CHAR MUST FOLLOW)
028400     02  CTLCHR40   PIC X  VALUE " ".
028500* AS WRITE CONTROL CHARCTER = X"40"= FSET   LEAVEKB  SILENT
028600* AS COPY CONTROL CHARACTER = X"40"= ATTRB ONLY
028700     02  CTLCHR41   PIC X  VALUE " ".
028800* AS WRITE CONTROL CHARCTER = X"41"= FRSET  LEAVEKB  SILENT
028900* AS COPY CONTROL CHARACTER = X"41"= DATA ONLY
029000     02  CTLCHR42   PIC X  VALUE "¡".
029100* AS WRITE CONTROL CHARCTER = X"42"= FSET   FREEKB   SILENT
029200* AS COPY CONTROL CHARACTER = X"42"= MAP ONLY
029300     02  CTLCHR43   PIC X  VALUE "¢".
029400* AS WRITE CONTROL CHARCTER = X"43"= FRSET  FREEKB   SILENT
029500* AS COPY CONTROL CHARACTER = X"43"= FULL BUFFER
029600     02  CTLCHR44   PIC X  VALUE "£".
029700* AS WRITE CONTROL CHARCTER = X"44"= FSET   LEAVEKB  ALARM
029800* AS COPY CONTROL CHARACTER = X"44"= ATTRB ONLY    + ALARM
029900     02  CTLCHR45   PIC X  VALUE "¤".
030000* AS WRITE CONTROL CHARCTER = X"45"= FRSET  LEAVEKB  ALARM
030100* AS COPY CONTROL CHARACTER = X"45"= DATA ONLY     + ALARM
030200     02  CTLCHR46   PIC X  VALUE "¥".
030300* AS WRITE CONTROL CHARCTER = X"46"= FSET   FREEKB   ALARM
030400* AS COPY CONTROL CHARACTER = X"46"= MAP ONLY      + ALARM
030500     02  CTLCHR47   PIC X  VALUE "¦".
030600* AS WRITE CONTROL CHARCTER = X"47"= FRSET  FREEKB   ALARM
030700* AS COPY CONTROL CHARACTER = X"47"= FULL BUFFER   + ALARM
030800 01  SCRBUFAD.
030900     05 FILLER PIC X(32) VALUE "   A B C D E F G H I [ . < ( + ]".
031000     05 FILLER PIC X(32) VALUE " & J K L M N O P Q R ! $ * ) ; ^".
031100     05 FILLER PIC X(32) VALUE " - / S T U V W X Y Z | , % _ > ?".
031200     05 FILLER PIC X(31) VALUE " 0 1 2 3 4 5 6 7 8 9 : # @ ' = ".
031300     05 FILLER PIC X     VALUE QUOTE.
031400     05 FILLER PIC X(32) VALUE "A AAABACADAEAFAGAHAIA[A.A<A(A+A]".
031500     05 FILLER PIC X(32) VALUE "A&AJAKALAMANAOAPAQARA!A$A*A)A;A^".
031600     05 FILLER PIC X(32) VALUE "A-A/ASATAUAVAWAXAYAZA|A,A%A_A>A?".
031700     05 FILLER PIC X(31) VALUE "A0A1A2A3A4A5A6A7A8A9A:A#A@A'A=A".
031800     05 FILLER PIC X     VALUE QUOTE.
031900     05 FILLER PIC X(32) VALUE "B BABBBCBDBEBFBGBHBIB[B.B<B(B+B]".
032000     05 FILLER PIC X(32) VALUE "B&BJBKBLBMBNBOBPBQBRB!B$B*B)B;B^".
032100     05 FILLER PIC X(32) VALUE "B-B/BSBTBUBVBWBXBYBZB|B,B%B_B>B?".
032200     05 FILLER PIC X(31) VALUE "B0B1B2B3B4B5B6B7B8B9B:B#B@B'B=B".
032300     05 FILLER PIC X     VALUE QUOTE.
032400     05 FILLER PIC X(32) VALUE "C CACBCCCDCECFCGCHCIC[C.C<C(C+C]".
032500     05 FILLER PIC X(32) VALUE "C&CJCKCLCMCNCOCPCQCRC!C$C*C)C;C^".
032600     05 FILLER PIC X(32) VALUE "C-C/CSCTCUCVCWCXCYCZC|C,C%C_C>C?".
032700     05 FILLER PIC X(31) VALUE "C0C1C2C3C4C5C6C7C8C9C:C#C@C'C=C".
032800     05 FILLER PIC X     VALUE QUOTE.
032900     05 FILLER PIC X(32) VALUE "D DADBDCDDDEDFDGDHDID[D.D<D(D+D]".
033000     05 FILLER PIC X(32) VALUE "D&DJDKDLDMDNDODPDQDRD!D$D*D)D;D^".
033100     05 FILLER PIC X(32) VALUE "D-D/DSDTDUDVDWDXDYDZD|D,D%D_D>D?".
033200     05 FILLER PIC X(31) VALUE "D0D1D2D3D4D5D6D7D8D9D:D#D@D'D=D".
033300     05 FILLER PIC X     VALUE QUOTE.
033400     05 FILLER PIC X(32) VALUE "E EAEBECEDEEEFEGEHEIE[E.E<E(E+E]".
033500     05 FILLER PIC X(32) VALUE "E&EJEKELEMENEOEPEQERE!E$E*E)E;E^".
033600     05 FILLER PIC X(32) VALUE "E-E/ESETEUEVEWEXEYEZE|E,E%E_E>E?".
033700     05 FILLER PIC X(31) VALUE "E0E1E2E3E4E5E6E7E8E9E:E#E@E'E=E".
033800     05 FILLER PIC X     VALUE QUOTE.
033900     05 FILLER PIC X(32) VALUE "F FAFBFCFDFEFFFGFHFIF[F.F<F(F+F]".
034000     05 FILLER PIC X(32) VALUE "F&FJFKFLFMFNFOFPFQFRF!F$F*F)F;F^".
034100     05 FILLER PIC X(32) VALUE "F-F/FSFTFUFVFWFXFYFZF|F,F%F_F>F?".
034200     05 FILLER PIC X(31) VALUE "F0F1F2F3F4F5F6F7F8F9F:F#F@F'F=F".
034300     05 FILLER PIC X     VALUE QUOTE.
034400     05 FILLER PIC X(32) VALUE "G GAGBGCGDGEGFGGGHGIG[G.G<G(G+G]".
034500     05 FILLER PIC X(32) VALUE "G&GJGKGLGMGNGOGPGQGRG!G$G*G)G;G^".
034600     05 FILLER PIC X(32) VALUE "G-G/GSGTGUGVGWGXGYGZG|G,G%G_G>G?".
034700     05 FILLER PIC X(31) VALUE "G0G1G2G3G4G5G6G7G8G9G:G#G@G'G=G".
034800     05 FILLER PIC X     VALUE QUOTE.
034900     05 FILLER PIC X(32) VALUE "H HAHBHCHDHEHFHGHHHIH[H.H<H(H+H]".
035000     05 FILLER PIC X(32) VALUE "H&HJHKHLHMHNHOHPHQHRH!H$H*H)H;H^".
035100     05 FILLER PIC X(32) VALUE "H-H/HSHTHUHVHWHXHYHZH|H,H%H_H>H?".
035200     05 FILLER PIC X(31) VALUE "H0H1H2H3H4H5H6H7H8H9H:H#H@H'H=H".
035300     05 FILLER PIC X     VALUE QUOTE.
035400     05 FILLER PIC X(32) VALUE "I IAIBICIDIEIFIGIHIII[I.I<I(I+I]".
035500     05 FILLER PIC X(32) VALUE "I&IJIKILIMINIOIPIQIRI!I$I*I)I;I^".
035600     05 FILLER PIC X(32) VALUE "I-I/ISITIUIVIWIXIYIZI|I,I%I_I>I?".
035700     05 FILLER PIC X(31) VALUE "I0I1I2I3I4I5I6I7I8I9I:I#I@I'I=I".
035800     05 FILLER PIC X     VALUE QUOTE.
035900     05 FILLER PIC X(32) VALUE "[ [A[B[C[D[E[F[G[H[I[[[.[<[([+[]".
036000     05 FILLER PIC X(32) VALUE "[&[J[K[L[M[N[O[P[Q[R[![$[*[)[;[^".
036100     05 FILLER PIC X(32) VALUE "[-[/[S[T[U[V[W[X[Y[Z[|[,[%[_[>[?".
036200     05 FILLER PIC X(31) VALUE "[0[1[2[3[4[5[6[7[8[9[:[#[@['[=[".
036300     05 FILLER PIC X     VALUE QUOTE.
036400     05 FILLER PIC X(32) VALUE ". .A.B.C.D.E.F.G.H.I.[...<.(.+.]".
036500     05 FILLER PIC X(32) VALUE ".&.J.K.L.M.N.O.P.Q.R.!.$.*.).;.^".
036600     05 FILLER PIC X(32) VALUE ".-./.S.T.U.V.W.X.Y.Z.|.,.%._.>.?".
036700     05 FILLER PIC X(31) VALUE ".0.1.2.3.4.5.6.7.8.9.:.#.@.'.=.".
036800     05 FILLER PIC X     VALUE QUOTE.
036900     05 FILLER PIC X(32) VALUE "< <A<B<C<D<E<F<G<H<I<[<.<<<(<+<]".
037000     05 FILLER PIC X(32) VALUE "<&<J<K<L<M<N<O<P<Q<R<!<$<*<)<;<^".
037100     05 FILLER PIC X(32) VALUE "<-</<S<T<U<V<W<X<Y<Z<|<,<%<_<><?".
037200     05 FILLER PIC X(31) VALUE "<0<1<2<3<4<5<6<7<8<9<:<#<@<'<=<".
037300     05 FILLER PIC X     VALUE QUOTE.
037400     05 FILLER PIC X(32) VALUE "( (A(B(C(D(E(F(G(H(I([(.(<(((+(]".
037500     05 FILLER PIC X(32) VALUE "(&(J(K(L(M(N(O(P(Q(R(!($(*()(;(^".
037600     05 FILLER PIC X(32) VALUE "(-(/(S(T(U(V(W(X(Y(Z(|(,(%(_(>(?".
037700     05 FILLER PIC X(31) VALUE "(0(1(2(3(4(5(6(7(8(9(:(#(@('(=(".
037800     05 FILLER PIC X     VALUE QUOTE.
037900     05 FILLER PIC X(32) VALUE "+ +A+B+C+D+E+F+G+H+I+[+.+<+(+++]".
038000     05 FILLER PIC X(32) VALUE "+&+J+K+L+M+N+O+P+Q+R+!+$+*+)+;+^".
038100     05 FILLER PIC X(32) VALUE "+-+/+S+T+U+V+W+X+Y+Z+|+,+%+_+>+?".
038200     05 FILLER PIC X(31) VALUE "+0+1+2+3+4+5+6+7+8+9+:+#+@+'+=+".
038300     05 FILLER PIC X     VALUE QUOTE.
038400     05 FILLER PIC X(32) VALUE "] ]A]B]C]D]E]F]G]H]I][].]<](]+]]".
038500     05 FILLER PIC X(32) VALUE "]&]J]K]L]M]N]O]P]Q]R]!]$]*])];]^".
038600     05 FILLER PIC X(32) VALUE "]-]/]S]T]U]V]W]X]Y]Z]|],]%]_]>]?".
038700     05 FILLER PIC X(31) VALUE "]0]1]2]3]4]5]6]7]8]9]:]#]@]']=]".
038800     05 FILLER PIC X     VALUE QUOTE.
038900     05 FILLER PIC X(32) VALUE "& &A&B&C&D&E&F&G&H&I&[&.&<&(&+&]".
039000     05 FILLER PIC X(32) VALUE "&&&J&K&L&M&N&O&P&Q&R&!&$&*&)&;&^".
039100     05 FILLER PIC X(32) VALUE "&-&/&S&T&U&V&W&X&Y&Z&|&,&%&_&>&?".
039200     05 FILLER PIC X(31) VALUE "&0&1&2&3&4&5&6&7&8&9&:&#&@&'&=&".
039300     05 FILLER PIC X     VALUE QUOTE.
039400     05 FILLER PIC X(32) VALUE "J JAJBJCJDJEJFJGJHJIJ[J.J<J(J+J]".
039500     05 FILLER PIC X(32) VALUE "J&JJJKJLJMJNJOJPJQJRJ!J$J*J)J;J^".
039600     05 FILLER PIC X(32) VALUE "J-J/JSJTJUJVJWJXJYJZJ|J,J%J_J>J?".
039700     05 FILLER PIC X(31) VALUE "J0J1J2J3J4J5J6J7J8J9J:J#J@J'J=J".
039800     05 FILLER PIC X     VALUE QUOTE.
039900     05 FILLER PIC X(32) VALUE "K KAKBKCKDKEKFKGKHKIK[K.K<K(K+K]".
040000     05 FILLER PIC X(32) VALUE "K&KJKKKLKMKNKOKPKQKRK!K$K*K)K;K^".
040100     05 FILLER PIC X(32) VALUE "K-K/KSKTKUKVKWKXKYKZK|K,K%K_K>K?".
040200     05 FILLER PIC X(31) VALUE "K0K1K2K3K4K5K6K7K8K9K:K#K@K'K=K".
040300     05 FILLER PIC X     VALUE QUOTE.
040400     05 FILLER PIC X(32) VALUE "L LALBLCLDLELFLGLHLIL[L.L<L(L+L]".
040500     05 FILLER PIC X(32) VALUE "L&LJLKLLLMLNLOLPLQLRL!L$L*L)L;L^".
040600     05 FILLER PIC X(32) VALUE "L-L/LSLTLULVLWLXLYLZL|L,L%L_L>L?".
040700     05 FILLER PIC X(31) VALUE "L0L1L2L3L4L5L6L7L8L9L:L#L@L'L=L".
040800     05 FILLER PIC X     VALUE QUOTE.
040900     05 FILLER PIC X(32) VALUE "M MAMBMCMDMEMFMGMHMIM[M.M<M(M+M]".
041000     05 FILLER PIC X(32) VALUE "M&MJMKMLMMMNMOMPMQMRM!M$M*M)M;M^".
041100     05 FILLER PIC X(32) VALUE "M-M/MSMTMUMVMWMXMYMZM|M,M%M_M>M?".
041200     05 FILLER PIC X(31) VALUE "M0M1M2M3M4M5M6M7M8M9M:M#M@M'M=M".
041300     05 FILLER PIC X     VALUE QUOTE.
041400     05 FILLER PIC X(32) VALUE "N NANBNCNDNENFNGNHNIN[N.N<N(N+N]".
041500     05 FILLER PIC X(32) VALUE "N&NJNKNLNMNNNONPNQNRN!N$N*N)N;N^".
041600     05 FILLER PIC X(32) VALUE "N-N/NSNTNUNVNWNXNYNZN|N,N%N_N>N?".
041700     05 FILLER PIC X(31) VALUE "N0N1N2N3N4N5N6N7N8N9N:N#N@N'N=N".
041800     05 FILLER PIC X     VALUE QUOTE.
041900     05 FILLER PIC X(32) VALUE "O OAOBOCODOEOFOGOHOIO[O.O<O(O+O]".
042000     05 FILLER PIC X(32) VALUE "O&OJOKOLOMONOOOPOQORO!O$O*O)O;O^".
042100     05 FILLER PIC X(32) VALUE "O-O/OSOTOUOVOWOXOYOZO|O,O%O_O>O?".
042200     05 FILLER PIC X(31) VALUE "O0O1O2O3O4O5O6O7O8O9O:O#O@O'O=O".
042300     05 FILLER PIC X     VALUE QUOTE.
042400     05 FILLER PIC X(32) VALUE "P PAPBPCPDPEPFPGPHPIP[P.P<P(P+P]".
042500     05 FILLER PIC X(32) VALUE "P&PJPKPLPMPNPOPPPQPRP!P$P*P)P;P^".
042600     05 FILLER PIC X(32) VALUE "P-P/PSPTPUPVPWPXPYPZP|P,P%P_P>P?".
042700     05 FILLER PIC X(31) VALUE "P0P1P2P3P4P5P6P7P8P9P:P#P@P'P=P".
042800     05 FILLER PIC X     VALUE QUOTE.
042900     05 FILLER PIC X(32) VALUE "Q QAQBQCQDQEQFQGQHQIQ[Q.Q<Q(Q+Q]".
043000     05 FILLER PIC X(32) VALUE "Q&QJQKQLQMQNQOQPQQQRQ!Q$Q*Q)Q;Q^".
043100     05 FILLER PIC X(32) VALUE "Q-Q/QSQTQUQVQWQXQYQZQ|Q,Q%Q_Q>Q?".
043200     05 FILLER PIC X(31) VALUE "Q0Q1Q2Q3Q4Q5Q6Q7Q8Q9Q:Q#Q@Q'Q=Q".
043300     05 FILLER PIC X     VALUE QUOTE.
043400     05 FILLER PIC X(32) VALUE "R RARBRCRDRERFRGRHRIR[R.R<R(R+R]".
043500     05 FILLER PIC X(32) VALUE "R&RJRKRLRMRNRORPRQRRR!R$R*R)R;R^".
043600     05 FILLER PIC X(32) VALUE "R-R/RSRTRURVRWRXRYRZR|R,R%R_R>R?".
043700     05 FILLER PIC X(31) VALUE "R0R1R2R3R4R5R6R7R8R9R:R#R@R'R=R".
043800     05 FILLER PIC X     VALUE QUOTE.
043900     05 FILLER PIC X(32) VALUE "! !A!B!C!D!E!F!G!H!I![!.!<!(!+!]".
044000     05 FILLER PIC X(32) VALUE "!&!J!K!L!M!N!O!P!Q!R!!!$!*!)!;!^".
044100     05 FILLER PIC X(32) VALUE "!-!/!S!T!U!V!W!X!Y!Z!|!,!%!_!>!?".
044200     05 FILLER PIC X(31) VALUE "!0!1!2!3!4!5!6!7!8!9!:!#!@!'!=!".
044300     05 FILLER PIC X     VALUE QUOTE.
044400     05 FILLER PIC X(32) VALUE "$ $A$B$C$D$E$F$G$H$I$[$.$<$($+$]".
044500     05 FILLER PIC X(32) VALUE "$&$J$K$L$M$N$O$P$Q$R$!$$$*$)$;$^".
044600     05 FILLER PIC X(32) VALUE "$-$/$S$T$U$V$W$X$Y$Z$|$,$%$_$>$?".
044700     05 FILLER PIC X(31) VALUE "$0$1$2$3$4$5$6$7$8$9$:$#$@$'$=$".
044800     05 FILLER PIC X     VALUE QUOTE.
044900     05 FILLER PIC X(32) VALUE "* *A*B*C*D*E*F*G*H*I*[*.*<*(*+*]".
045000     05 FILLER PIC X(32) VALUE "*&*J*K*L*M*N*O*P*Q*R*!*$***)*;*^".
045100     05 FILLER PIC X(32) VALUE "*-*/*S*T*U*V*W*X*Y*Z*|*,*%*_*>*?".
045200     05 FILLER PIC X(31) VALUE "*0*1*2*3*4*5*6*7*8*9*:*#*@*'*=*".
045300     05 FILLER PIC X     VALUE QUOTE.
045400     05 FILLER PIC X(32) VALUE ") )A)B)C)D)E)F)G)H)I)[).)<)()+)]".
045500     05 FILLER PIC X(32) VALUE ")&)J)K)L)M)N)O)P)Q)R)!)$)*)));)^".
045600     05 FILLER PIC X(32) VALUE ")-)/)S)T)U)V)W)X)Y)Z)|),)%)_)>)?".
045700     05 FILLER PIC X(31) VALUE ")0)1)2)3)4)5)6)7)8)9):)#)@)')=)".
045800     05 FILLER PIC X     VALUE QUOTE.
045900 01  FILLER REDEFINES SCRBUFAD.
046000     05  FILLER OCCURS 24 TIMES.
046100         10  FILLER OCCURS 80 TIMES.
046200*                                   BUFFER (ROW-COLUMN) ADDRESSING
046300             15  BUFAD           PIC XX.
046400 01  FILLER REDEFINES SCRBUFAD.
046500     05  FILLER OCCURS 1920 TIMES.
046600*                                   RELATIVE (MAP BYTE) ADDRESSING
046700         10  RMBAD               PIC XX.
046800 01  DFHAID.
046900     02  DFHPF1                  PIC X        VALUE "1".
047000     02  DFHPF2                  PIC X        VALUE "2".
047100     02  DFHPF3                  PIC X        VALUE "3".
047200     02  DFHPF4                  PIC X        VALUE "4".
047300     02  DFHPF5                  PIC X        VALUE "5".
047400     02  DFHPF6                  PIC X        VALUE "6".
047500     02  DFHPF7                  PIC X        VALUE "7".
047600     02  DFHPF8                  PIC X        VALUE "8".
047700     02  DFHPF9                  PIC X        VALUE "9".
047800     02  DFHPF10                 PIC X        VALUE ":".
047900     02  DFHPF11                 PIC X        VALUE "#".
048000     02  DFHPF12                 PIC X        VALUE "@".
048100     02  DFHPA1                  PIC X        VALUE "%".
048200     02  DFHPA2                  PIC X        VALUE ">".
048300     02  DFHCLEAR                PIC X        VALUE "_".
048400     02  DFHENTER                PIC X        VALUE "'".
048500******************************************************************
048600*                                                                *
048700*    USER ROUTING LIST AREA                                      *
048800*                                                                *
048900******************************************************************
049000 01  URLENTRY.
049100     02  URLFIRST.
049200         03  URLTRMID        PIC X(4).
049300         03  URLLDCMN        PIC X(2).
049400         03  URLOPID         PIC X(3).
049500         03  URLTSF          PIC X.
049600             88  URLSKIP     VALUES "Ã", "h", "Ê", "Ñ", "{", "d".
049700             88  URLITI      VALUE  "{".
049800             88  URLNS       VALUE  "Ñ".
049900             88  URLONSO     VALUES "Ê", "".
050000             88  URLSOUST    VALUE  "h".
050100             88  URLINVMN    VALUE  "d".
050200         03  FILLER          PIC X(6).
050300     02  URLCHAIN.
050400         03  URLCHIND         PIC S9(4) USAGE IS COMPUTATIONAL.
050500             88  URLEND      VALUE -1.
050600             88  URLCONT     VALUE -2.
050700         03  FILLER           PIC S9(4) USAGE IS COMPUTATIONAL.
050800         03  URLCHADR         PIC S9(8) USAGE IS COMPUTATIONAL.
050900******************************************************************
051000*                                                                *
051100*    EXECUTE INTERFACE VARIABLES                                 *
051200*                                                                *
051300******************************************************************
051400 PROCEDURE DIVISION.
051500******************************************************************
051600*                                                                *
051700*    HOUSEKEEPING                                                *
051800*                                                                *
051900******************************************************************
052000     EXEC CICS HANDLE CONDITION ENDDATA(A00-INITIALIZE)
052100                                ERROR(X00-ERROR)
052200                                END-EXEC.
052300     EXEC CICS HANDLE AID ANYKEY
052400                          END-EXEC.
052500*
052600     EXEC CICS RETRIEVE INTO(INTERVAL-LINK)
052700                        LENGTH(INTERVAL-LENG)
052800                        RTERMID(INTERVAL-TERM)
052900                        END-EXEC.
053000     EXEC CICS HANDLE CONDITION ENDDATA
053100                                END-EXEC.
053200*
053300     IF INTERVAL-PREVMV = "SP"
053400         GO TO A10-INSTRUCTIONS.
053500*
053600     GO TO B00-INPROCESS.
053700******************************************************************
053800*                                                                *
053900*    INITIALIZE GAMEAREAS & GET PLAYER NAMES                     *
054000*                                                                *
054100******************************************************************
054200 A00-INITIALIZE.
054300     EXEC CICS HANDLE CONDITION ENDDATA
054400                                END-EXEC.
054500*
054600     EXEC CICS RECEIVE INTO(TERM-IOA)
054700                       LENGTH(I-O-LEN)
054800                       END-EXEC.
054900     EXEC CICS ASKTIME END-EXEC.
055000     IF (EIBTIME > 0065959 AND EIBTIME < 0073201)
055100     OR (EIBTIME > 0115959 AND EIBTIME < 0123201)
055200     OR (EIBTIME > 0165959 AND EIBTIME < 0173201)
055300         NEXT SENTENCE
055400     ELSE
055500         MOVE "GAME TIMES: 7-7:30, 12-12:30 OR 5-5:30" TO OUT-MSG
055600         MOVE  +0038   TO I-O-LEN
055700         EXEC CICS SEND FROM(OUT-MSG)
055800                        LENGTH(I-O-LEN)
055900                        ERASE
056000                        END-EXEC
056100         EXEC CICS RETURN END-EXEC.
056200     MOVE   TERM-IOA   TO INP-MSG.
056300     IF INP-TRAN NOT = OTHELLO
056400         MOVE "START TRANSACTION FROM A CLEAR SCREEN" TO OUT-MSG
056500         MOVE  +0037   TO I-O-LEN
056600         EXEC CICS SEND FROM(OUT-MSG)
056700                        LENGTH(I-O-LEN)
056800                        ERASE
056900                        END-EXEC
057000         EXEC CICS RETURN END-EXEC.
057100*
057200     MOVE  LOW-VALUES  TO URLENTRY.
057300     MOVE    SPACES    TO URLFIRST.
057400     MOVE   INP-TERM   TO URLTRMID.
057500     MOVE      -1      TO URLCHIND.
057600     MOVE    ZEROES    TO URLCHADR.
057700     EXEC CICS ROUTE LIST(URLENTRY)
057800                     ERRTERM(EIBTRMID)
057900                     END-EXEC.
058000     IF URLITI GO TO Z00-EOJ.
058100*
058200     MOVE   EIBTRMID   TO ACK-TERM.
058300     EXEC CICS SEND TEXT FROM(ACK-MESG)
058400                         LENGTH(ACK-LENG)
058500                         CURSOR(80)
058600                         PAGING
058700                         ACCUM
058800                         ALARM
058900                         ERASE
059000                         FREEKB
059100                         END-EXEC.
059200     EXEC CICS SEND PAGE END-EXEC.
059300*
059400     EXEC CICS DELETEQ TS QUEUE(QUEUE-NAME)
059500                          NOHANDLE
059600                          END-EXEC.
059700*
059800     MOVE   INP-TERM   TO INTERVAL-TERM.
059900     MOVE      2       TO INTERVAL-PLAYNO.
060000     MOVE    ZEROES    TO INTERVAL-MOVENO.
060100     MOVE    SPACES    TO INTERVAL-MVMODE.
060200     MOVE     "SP"     TO INTERVAL-PREVMV.
060300     MOVE  ALL ZEROES  TO INTERVAL-PIECES (1) INTERVAL-PIECES (2).
060400     MOVE  LOW-VALUES  TO INTERVAL-PNAMES (1) INTERVAL-PNAMES (2).
060500     MOVE  LOW-VALUES  TO INTERVAL-PBOARD.
060600*
060700 A10-INSTRUCTIONS.
060800     MOVE   SCRSETBA   TO MESSAGO-SBA ERASEO-SBA.
060900     MOVE BUFAD (4 14) TO MESSAGO-BAD ERASEO-BAD.
061000     MOVE   SCRSTRTF   TO MESSAGO-STF.
061100     MOVE   SCRASBRT   TO MESSAGO-ATR.
061200*
061300     IF INTERVAL-PLAYNO = 1
061400         THEN MOVE  2  TO INTERVAL-PLAYNO PLAYR, MOVE 1 TO ENEMY
061500         ELSE MOVE  1  TO INTERVAL-PLAYNO PLAYR, MOVE 2 TO ENEMY.
061600*
061700     MOVE  LOW-VALUES  TO DATAMAPO.
061800     MOVE PLAYER (PLAYR 1) TO TITTLE1O.
061900     MOVE PLAYER (ENEMY 2) TO TITTLE2O.
062000     MOVE      -1      TO PLAYER1L.
062100     MOVE   SCRUNBRF   TO PLAYER1A.
062200     MOVE INTERVAL-PNAMES (PLAYR) TO PLAYER1O.
062300     MOVE INTERVAL-PNAMES (ENEMY) TO PLAYER2O.
062400     MOVE INTERVAL-PIECES (PLAYR) TO PIECES1O.
062500     MOVE INTERVAL-PIECES (ENEMY) TO PIECES2O.
062600     MOVE   SCRASKIP   TO MOVEIS1A.
062700     MOVE  LOW-VALUES  TO MOVEIS1O.
062800     MOVE INTERVAL-PREVMV TO MOVEIS2O.
062900*
063000     EXEC CICS SEND MAP(DATAMAP)
063100                    FROM(DATAMAPO)
063200                    MAPSET(OTHEMAP)
063300                    TERMINAL
063400                    FREEKB
063500                    FRSET
063600                    ERASE
063700                    WAIT
063800                    ALARM
063900                    CURSOR
064000                    END-EXEC.
064100     PERFORM A20-REPEAT 5 TIMES.
064200 A20-REPEAT.
064300     EXEC CICS SEND FROM(MESSAGO)
064400                    LENGTH(MESSAGL)
064500                    WAIT
064600                    END-EXEC.
064700     EXEC CICS SEND FROM(ERASEO)
064800                    LENGTH(ERASEL)
064900                    WAIT
065000                    END-EXEC.
065100 A30-RESEND.
065200     EXEC CICS SEND FROM(MESSAGO)
065300                    LENGTH(MESSAGL)
065400                    CTLCHAR(CTLCHR46)
065500                    WAIT
065600                    END-EXEC.
065700     EXEC CICS RECEIVE MAP(DATAMAP)
065800                       INTO(DATAMAPI)
065900                       MAPSET(OTHEMAP)
066000                       TERMINAL
066100                       ASIS
066200                       END-EXEC.
066300     EXEC CICS SEND FROM(ERASEO)
066400                    LENGTH(ERASEL)
066500                    CTLCHAR(CTLCHR41)
066600                    WAIT
066700                    END-EXEC.
066800*
066900 A40-FINISH-SETUP.
067000     EXAMINE PLAYER1I REPLACING ALL "_" BY " ".
067100     IF PLAYER1I = LOW-VALUES OR SPACES
067200         GO TO A30-RESEND.
067300     IF PLAYR = 2
067400         MOVE   "PL"   TO INTERVAL-PREVMV
067500         MOVE   "X"    TO POSXO (4 4) POSXO (5 5)
067600         MOVE   "O"    TO POSXO (4 5) POSXO (5 4)
067700         MOVE    02    TO INTERVAL-PIECES (1) INTERVAL-PIECES (2).
067800         MOVE PLAYING-BOARD TO INTERVAL-PBOARD.
067900     MOVE   PLAYER1I   TO INTERVAL-PNAMES (PLAYR).
068000*
068100     EXEC CICS START TRANSID(OTHELLO)
068200                     FROM(INTERVAL-LINK)
068300                     LENGTH(INTERVAL-LENG)
068400                     TERMID(INTERVAL-TERM)
068500                     RTERMID(EIBTRMID)
068600                     END-EXEC.
068700*
068800     EXEC CICS RETURN END-EXEC.
068900******************************************************************
069000*                                                                *
069100*    END OF SETUP ROUTINE                                        *
069200*                                                                *
069300******************************************************************
069400 EJECT
069500******************************************************************
069600*                                                                *
069700*    BEGIN CURRENT TURN OF PLAY                                  *
069800*                                                                *
069900******************************************************************
070000 B00-INPROCESS.
070100     IF INTERVAL-PLAYNO = 1
070200         MOVE    2     TO PLAYR INTERVAL-PLAYNO
070300         MOVE   "O"    TO PIECE
070400         MOVE    1     TO ENEMY
070500     ELSE
070600         MOVE    1     TO PLAYR INTERVAL-PLAYNO
070700         MOVE   "X"    TO PIECE
070800         MOVE    2     TO ENEMY.
070900*
071000     MOVE  LOW-VALUES  TO DATAMAPO.
071100     MOVE PLAYER (PLAYR 1) TO TITTLE1O.
071200     MOVE PLAYER (ENEMY 2) TO TITTLE2O.
071300     MOVE INTERVAL-PNAMES (PLAYR) TO PLAYER1O.
071400     MOVE INTERVAL-PNAMES (ENEMY) TO PLAYER2O.
071500     MOVE INTERVAL-PIECES (PLAYR) TO PIECES1O.
071600     MOVE INTERVAL-PIECES (ENEMY) TO PIECES2O.
071700     MOVE   LOW-VALUES    TO MOVEIS1O.
071800     MOVE INTERVAL-PREVMV TO MOVEIS2O.
071900     MOVE INTERVAL-PBOARD TO PLAYING-BOARD.
072000     MOVE    +0046     TO ERASEL.
072100     MOVE    +0047     TO MESSAGL.
072200     MOVE   SCRSETBA   TO MESSAGO-SBA ERASEO-SBA.
072300     MOVE BUFAD (4 19) TO MESSAGO-BAD ERASEO-BAD.
072400     MOVE   SCRSTRTF   TO MESSAGO-STF.
072500     MOVE   SCRASBRT   TO MESSAGO-ATR.
072600     IF "GO" = MOVEIS2O
072700         GO TO Y00-END-OF-GAME.
072800*
072900 B10-RETRY.
073000     IF INTERVAL-PNAMES (PLAYR) = "OTHEPGM0" OR "OTHEPGM1"
073100                               OR "OTHEPGM2" OR "OTHEPGM3"
073200                               OR "OTHEPGM4" OR "OTHEPGM5"
073300                               OR "OTHEPGM6" OR "OTHEPGM7"
073400                               OR "OTHEPGM8" OR "OTHEPGM9"
073500         GO TO B20-SEND-SCREEN.
073600*
073700     EXEC CICS LINK
073800               PROGRAM(INTERVAL-PNAMES (PLAYR))
073900               COMMAREA(INTERVAL-LINK)
074000               LENGTH(INTERVAL-LENG)
074100               NOHANDLE
074200               END-EXEC.
074300     IF EIBRCODE = LOW-VALUES
074400         MOVE INTERVAL-PREVMV TO MOVEIS1O
074500         GO TO B30-PROCESS-INPUT.
074600*
074700 B20-SEND-SCREEN.
074800     EXEC CICS SEND MAP(DATAMAP)
074900                    FROM(DATAMAPO)
075000                    MAPSET(OTHEMAP)
075100                    TERMINAL
075200                    FREEKB
075300                    FRSET
075400                    ALARM
075500                    END-EXEC.
075600     EXEC CICS RECEIVE MAP(DATAMAP)
075700                       INTO(DATAMAPI)
075800                       MAPSET(OTHEMAP)
075900                       TERMINAL
076000                       ASIS
076100                       END-EXEC.
076200     EXEC CICS SEND FROM(ERASEO)
076300                    LENGTH(ERASEL)
076400                    CTLCHAR(CTLCHR41)
076500                    END-EXEC.
076600*
076700 B30-PROCESS-INPUT.
076800     IF EIBAID = DFHPF1
076900         IF MOVEIS2O = "BW" OR "FW"
077000             SUBTRACT  1   FROM INTERVAL-MOVENO
077100             EXEC CICS READQ TS QUEUE(QUEUE-NAME)
077200                                INTO(PLAYING-BOARD)
077300                                LENGTH(QUEUE-RLEN)
077400                                ITEM(INTERVAL-MOVENO)
077500                                END-EXEC
077600             MOVE "BW" TO MOVEIS1O
077700             MOVE LOW-VALUES TO INTERVAL-MVMODE
077800             MOVE ZEROES TO PIECES1O PIECES2O
077900             IF PLAYR = 1
078000                 INSPECT PLAYING-BOARD
078100                     TALLYING PIECES1O FOR ALL "X"
078200                 INSPECT PLAYING-BOARD
078300                     TALLYING PIECES2O FOR ALL "O"
078400                 GO TO B90-START-TRANSACTION
078500             ELSE
078600                 INSPECT PLAYING-BOARD
078700                     TALLYING PIECES1O FOR ALL "O"
078800                 INSPECT PLAYING-BOARD
078900                     TALLYING PIECES2O FOR ALL "X"
079000                 GO TO B90-START-TRANSACTION.
079100     IF EIBAID = DFHPF12
079200         IF MOVEIS2O = "FW" OR "BW"
079300             ADD   1   TO INTERVAL-MOVENO
079400             EXEC CICS READQ TS QUEUE(QUEUE-NAME)
079500                                INTO(PLAYING-BOARD)
079600                                LENGTH(QUEUE-RLEN)
079700                                ITEM(INTERVAL-MOVENO)
079800                                END-EXEC
079900             MOVE "FW" TO MOVEIS1O
080000             MOVE HIGH-VALUES TO INTERVAL-MVMODE
080100             MOVE ZEROES TO PIECES1O PIECES2O
080200             IF PLAYR = 1
080300                 INSPECT PLAYING-BOARD
080400                     TALLYING PIECES1O FOR ALL "X"
080500                 INSPECT PLAYING-BOARD
080600                     TALLYING PIECES2O FOR ALL "O"
080700                 GO TO B90-START-TRANSACTION
080800             ELSE
080900                 INSPECT PLAYING-BOARD
081000                     TALLYING PIECES1O FOR ALL "O"
081100                 INSPECT PLAYING-BOARD
081200                     TALLYING PIECES2O FOR ALL "X"
081300                 GO TO B90-START-TRANSACTION.
081400     IF MOVEIS2O = "FW" OR "BW"
081500         MOVE   "F"    TO INTERVAL-MVMODE.
081600*
081700     IF MOVEIS1I = "__" OR SPACES OR LOW-VALUES
081800         MOVE  ZEROES  TO MOVER MOVEC
081900         GO TO B40-CHECK-VALID-MOVE.
082000*
082100     TRANSFORM MOVEIS1I FROM LOWER-CASE TO UPPER-CASE.
082200     MOVE   MOVEIS1I    TO CHECK-MOVE.
082300     IF MOVEBYT1 IS NOT NUMERIC
082400       IF MOVEBYT1 < "A" OR > "I"
082500         MOVE LOW-VALUES TO MOVEIS1O
082600         MOVE "** COL LETTER NOT IN RANGE 'A' THRU 'I' **"
082700             TO MESSAGO-MSG
082800         EXEC CICS SEND FROM(MESSAGO)
082900                        LENGTH(MESSAGL)
083000                        CTLCHAR(CTLCHR46)
083100                        END-EXEC
083200         GO TO B20-SEND-SCREEN
083300       ELSE
083400         IF MOVEBYT2 IS NOT NUMERIC
083500           MOVE LOW-VALUES TO MOVEIS1O
083600           MOVE "** NO ROW NUMBER FOUND IN YOUR RESPONSE **"
083700               TO MESSAGO-MSG
083800           EXEC CICS SEND FROM(MESSAGO)
083900                          LENGTH(MESSAGL)
084000                          CTLCHAR(CTLCHR46)
084100                          END-EXEC
084200           GO TO B20-SEND-SCREEN
084300         ELSE
084400           IF MOVEBYT2 = ZERO OR 9
084500             MOVE LOW-VALUES TO MOVEIS1O
084600             MOVE "** ROW NUMBER NOT IN RANGE '1' THRU '8' **"
084700                 TO MESSAGO-MSG
084800             EXEC CICS SEND FROM(MESSAGO)
084900                            LENGTH(MESSAGL)
085000                            CTLCHAR(CTLCHR46)
085100                            END-EXEC
085200             GO TO B20-SEND-SCREEN
085300           ELSE
085400             MOVE MOVEBYT2 TO MOVER
085500             TRANSFORM MOVEBYT1 FROM "ABCDEFGH" TO "12345678"
085600             MOVE MOVEBYT1 TO MOVEC
085700     ELSE
085800       IF MOVEBYT1 = ZERO OR 9
085900         MOVE LOW-VALUES TO MOVEIS1O
086000         MOVE "** ROW NUMBER NOT IN RANGE '1' THRU '8' **"
086100             TO MESSAGO-MSG
086200         EXEC CICS SEND FROM(MESSAGO)
086300                        LENGTH(MESSAGL)
086400                        CTLCHAR(CTLCHR46)
086500                        END-EXEC
086600         GO TO B20-SEND-SCREEN
086700       ELSE
086800         IF MOVEBYT2 < "A" OR > "I"
086900           MOVE LOW-VALUES TO MOVEIS1O
087000           MOVE "** COL LETTER NOT IN RANGE 'A' THRU 'I' **"
087100               TO MESSAGO-MSG
087200           EXEC CICS SEND FROM(MESSAGO)
087300                          LENGTH(MESSAGL)
087400                          CTLCHAR(CTLCHR46)
087500                          END-EXEC
087600           GO TO B20-SEND-SCREEN
087700         ELSE
087800           MOVE MOVEBYT1 TO MOVER
087900           TRANSFORM MOVEBYT2 FROM "ABCDEFGH" TO "12345678"
088000           MOVE MOVEBYT2 TO MOVEC.
088100*
088200     IF POSXI (MOVER MOVEC) NOT = "."
088300         MOVE LOW-VALUES TO MOVEIS1O
088400         MOVE "* POSITION TAKEN, MAKE ANOTHER SELECTION *"
088500             TO MESSAGO-MSG
088600         EXEC CICS SEND FROM(MESSAGO)
088700                        LENGTH(MESSAGL)
088800                        CTLCHAR(CTLCHR46)
088900                        END-EXEC
089000         GO TO B20-SEND-SCREEN.
089100*
089200 B40-CHECK-VALID-MOVE.
089300     PERFORM VALIDATE-MOVE THRU EXIT-VALIDATE-MOVE.
089400     IF MOVE-STATUS = "NO"
089500         IF (INTERVAL-PNAMES (PLAYR) = "OTHEPGM0" OR "OTHEPGM1"
089600                                    OR "OTHEPGM2" OR "OTHEPGM3"
089700                                    OR "OTHEPGM4" OR "OTHEPGM5"
089800                                    OR "OTHEPGM6" OR "OTHEPGM7"
089900                                    OR "OTHEPGM8" OR "OTHEPGM9")
090000         AND (MOVEIS1I = "__" OR SPACES OR LOW-VALUES)
090100             GO TO B10-RETRY
090200         ELSE
090300         MOVE LOW-VALUES TO MOVEIS1O
090400         MOVE "** INVALID MOVE, OR NOT ALLOWED TO PASS **"
090500             TO MESSAGO-MSG
090600         EXEC CICS SEND FROM(MESSAGO)
090700                        LENGTH(MESSAGL)
090800                        CTLCHAR(CTLCHR46)
090900                        END-EXEC
091000         GO TO B20-SEND-SCREEN.
091100*
091200     ADD       1       TO INTERVAL-MOVENO.
091300 B80-REWRITE.
091400     IF INTERVAL-MVMODE = SPACES
091500         EXEC CICS WRITEQ TS QUEUE(QUEUE-NAME)
091600                             FROM(PLAYING-BOARD)
091700                             LENGTH(QUEUE-RLEN)
091800                             ITEM(INTERVAL-MOVENO)
091900                             AUXILIARY
092000                             END-EXEC.
092100     IF INTERVAL-MVMODE = "F"
092200         EXEC CICS WRITEQ TS QUEUE(QUEUE-NAME)
092300                             FROM(PLAYING-BOARD)
092400                             LENGTH(QUEUE-RLEN)
092500                             ITEM(INTERVAL-MOVENO)
092600                             AUXILIARY
092700                             NOHANDLE
092800                             REWRITE
092900                             END-EXEC
093000         IF EIBRCODE NOT = LOW-VALUES
093100             MOVE SPACES TO INTERVAL-MVMODE
093200             GO TO B80-REWRITE.
093300*
093400     IF ZEROES = MOVER AND MOVEC
093500         MOVE "PS" TO MOVEIS1O
093600         IF MOVEIS2I = "PS"
093700             MOVE "GO" TO MOVEIS1O.
093800     IF MOVEIS1O NOT = "PS" AND "GO"
093900         COMPUTE PIECES1O = INTERVAL-PIECES (PLAYR) + FLIP-CNT + 1
094000         COMPUTE PIECES2O = INTERVAL-PIECES (ENEMY) - FLIP-CNT.
094100     IF PIECES1I + PIECES2I = 64
094200         MOVE "GO" TO MOVEIS1O.
094300*
094400 B90-START-TRANSACTION.
094500     MOVE   MOVEIS1I   TO INTERVAL-PREVMV.
094600     MOVE   PIECES1I   TO INTERVAL-PIECES (PLAYR).
094700     MOVE   PIECES2I   TO INTERVAL-PIECES (ENEMY).
094800     MOVE PLAYING-BOARD TO INTERVAL-PBOARD.
094900     EXEC CICS START TRANSID(OTHELLO)
095000                     FROM(INTERVAL-LINK)
095100                     LENGTH(INTERVAL-LENG)
095200                     TERMID(INTERVAL-TERM)
095300                     RTERMID(EIBTRMID)
095400                     END-EXEC.
095500*
095600     IF "GO" = MOVEIS1O
095700         GO TO Y00-END-OF-GAME.
095800*
095900     IF INTERVAL-PLAYNO = 1
096000         MOVE   "O"    TO PIECE
096100     ELSE
096200         MOVE   "X"    TO PIECE.
096300     MOVE    ZEROES    TO MOVER MOVEC.
096400     PERFORM VALIDATE-MOVE THRU EXIT-VALIDATE-MOVE.
096500     EXEC CICS SEND MAP(DATAMAP)
096600                    FROM(DATAMAPO)
096700                    MAPSET(OTHEMAP)
096800                    TERMINAL
096900                    FREEKB
097000                    FRSET
097100                    END-EXEC.
097200*
097300     EXEC CICS RETURN END-EXEC.
097400******************************************************************
097500*                                                                *
097600*    END CURRENT TURN OF PLAY                                    *
097700*                                                                *
097800******************************************************************
097900 EJECT
098000******************************************************************
098100*                                                                *
098200*    PERFORMED ROUTINES                                          *
098300*                                                                *
098400******************************************************************
098500 VALIDATE-MOVE.
098600     MOVE PIECE TO CURRENT-PLAYER.
098700     IF CURRENT-PLAYER = "X"
098800         MOVE "O" TO OPPOSING-PLAYER
098900     ELSE
099000         MOVE "X" TO OPPOSING-PLAYER.
099100     IF MOVER > 0 AND MOVEC > 0
099200         PERFORM MAKE-A-MOVE
099300     ELSE
099400         PERFORM WANT-TO-PASS
099500         MOVE ZEROES TO MOVER MOVEC.
099600 EXIT-VALIDATE-MOVE.
099700     EXIT.
099800 SKIP3
099900 MAKE-A-MOVE.
100000     MOVE "NO" TO MOVE-STATUS.
100100     MOVE ZERO TO FLIP-CNT.
100200     PERFORM MOVE-CHECK THRU EXIT-MOVE-CHECK
100300         VARYING DIRECTION FROM 1 BY 1
100400         UNTIL DIRECTION > 8.
100500     IF MOVE-STATUS = "OK"
100600         MOVE SCRASBRF       TO POSXA (MOVER MOVEC)
100700         MOVE CURRENT-PLAYER TO POSXO (MOVER MOVEC).
100800 SKIP1
100900 MOVE-CHECK.
101000     MOVE MOVER TO SUBR.
101100     MOVE MOVEC TO SUBC.
101200     MOVE 0 TO FOUND-OPPONENT.
101300 MOVE-LOOP.
101400     ADD DIR-TBLR (DIRECTION) TO SUBR.
101500     ADD DIR-TBLC (DIRECTION) TO SUBC.
101600     IF SUBR < 1 OR > 8
101700         GO TO EXIT-MOVE-CHECK.
101800     IF SUBC < 1 OR > 8
101900         GO TO EXIT-MOVE-CHECK.
102000     IF POSXI (SUBR SUBC) = CURRENT-PLAYER
102100         IF FOUND-OPPONENT = 0
102200             GO TO EXIT-MOVE-CHECK
102300         ELSE
102400             MOVE "OK" TO MOVE-STATUS
102500             PERFORM FLIP-LOOP THRU EXIT-FLIP-LOOP
102600             GO TO EXIT-MOVE-CHECK
102700     ELSE
102800         IF POSXI (SUBR SUBC) = OPPOSING-PLAYER
102900             MOVE 1 TO FOUND-OPPONENT
103000             GO TO MOVE-LOOP.
103100 EXIT-MOVE-CHECK.
103200     EXIT.
103300 SKIP2
103400 FLIP-LOOP.
103500     COMPUTE SUBR = SUBR - DIR-TBLR (DIRECTION).
103600     COMPUTE SUBC = SUBC - DIR-TBLC (DIRECTION).
103700     IF SUBR = MOVER AND SUBC = MOVEC
103800         GO TO EXIT-FLIP-LOOP.
103900     MOVE SCRASBRF       TO POSXA (SUBR SUBC).
104000     MOVE CURRENT-PLAYER TO POSXO (SUBR SUBC).
104100     ADD 1 TO FLIP-CNT.
104200     GO TO FLIP-LOOP.
104300 EXIT-FLIP-LOOP.
104400     EXIT.
104500 SKIP3
104600 WANT-TO-PASS.
104700     MOVE "OK" TO MOVE-STATUS.
104800     PERFORM CHECK-FOR-PASS THRU EXIT-CHECK-FOR-PASS
104900         VARYING MOVER FROM 8 BY -1
105000         UNTIL   MOVER < 1
105100         AFTER   MOVEC FROM 8 BY -1
105200         UNTIL   MOVEC < 1.
105300 SKIP1
105400 CHECK-FOR-PASS.
105500     IF POSXI (MOVER MOVEC) = CURRENT-PLAYER
105600         PERFORM LOOK-AROUND THRU EXIT-LOOK-AROUND
105700             VARYING DIRECTION FROM 1 BY 1
105800             UNTIL DIRECTION > 8.
105900 EXIT-CHECK-FOR-PASS.
106000     EXIT.
106100 SKIP2
106200 LOOK-AROUND.
106300     MOVE MOVER TO SUBR.
106400     MOVE MOVEC TO SUBC.
106500     MOVE 0 TO FOUND-OPPONENT.
106600 LOOK-LOOP.
106700     ADD DIR-TBLR (DIRECTION) TO SUBR.
106800     ADD DIR-TBLC (DIRECTION) TO SUBC.
106900     IF SUBR > 8 OR < 1
107000         GO TO EXIT-LOOK-AROUND.
107100     IF SUBC > 8 OR < 1
107200         GO TO EXIT-LOOK-AROUND.
107300     IF POSXI (SUBR SUBC) = OPPOSING-PLAYER
107400         MOVE 1 TO FOUND-OPPONENT
107500         GO TO LOOK-LOOP
107600     ELSE
107700         IF POSXI (SUBR SUBC) = CURRENT-PLAYER
107800             GO TO EXIT-LOOK-AROUND
107900         ELSE
108000             IF FOUND-OPPONENT = 1
108100                 MOVE "NO" TO MOVE-STATUS
108200                 MOVE SCRASBRF TO POSXA (SUBR SUBC).
108300 EXIT-LOOK-AROUND.
108400     EXIT.
108500 EJECT
108600******************************************************************
108700*                                                                *
108800*    ERROR EXIT ROUTINE               (HANDLE CONDITION EXIT)    *
108900*                                                                *
109000******************************************************************
109100 X00-ERROR.
109200     MOVE      03      TO RQC-CONVRT.
109300     MOVE   EIBRCODE   TO INP-CONVRT.
109400     MOVE      02      TO LEN-CONVRT.
109500 PROC-CONVRT.
109600     MOVE     "00"     TO RTC-CONVRT.
109700     GO TO  CHR-TO-DEC-CONVRT      CHR-TO-HEX-CONVRT
109800            CHR-TO-HEX-STR-CONVRT  HEX-TO-DEC-CONVRT
109900            HEX-TO-CHR-CONVRT      HEX-TO-CHR-STR-CONVRT
110000            DEC-TO-HEX-CONVRT      DEC-TO-CHR-CONVRT
110100            ADR-TO-DEC-CONVRT      DEC-TO-ADR-CONVRT
110200         DEPENDING ON RQC-CONVRT.
110300     MOVE     "01"     TO RTC-CONVRT.
110400     GO TO EXIT-CONVRT.
110500 DEC-TO-ADR-CONVRT.
110600     IF DEC-CONVRT > 4581298448
110700         MOVE "04"     TO RTC-CONVRT
110800         GO TO EXIT-CONVRT.
110900     MOVE    SPACES    TO AREA-I-CONVRT.
111000     PERFORM DECM-ADDRESS-CONVRT THRU DECM-ADDR-CONVRT
111100         VARYING SUBBT-CONVRT FROM 1 BY 1 UNTIL
111200             SUBBT-CONVRT > 8 OR RTC-CONVRT NOT = "00".
111300     MOVE AREA-I-CONVRT TO ADR-CONVRT.
111400     GO TO EXIT-CONVRT.
111500 ADR-TO-DEC-CONVRT.
111600     MOVE    ZEROES    TO DEC-CONVRT.
111700     MOVE  ADR-CONVRT  TO AREA-I-CONVRT.
111800     PERFORM ADDRESS-DECM-CONVRT THRU ADDR-DECM-CONVRT
111900         VARYING SUBBT-CONVRT FROM 1 BY 1 UNTIL
112000             SUBBT-CONVRT > 8 OR RTC-CONVRT NOT = "00".
112100     GO TO EXIT-CONVRT.
112200 DEC-TO-CHR-CONVRT.
112300     IF DEC-CONVRT > 256
112400         MOVE "04"     TO RTC-CONVRT
112500         GO TO EXIT-CONVRT.
112600     MOVE  DEC-CONVRT  TO DECHEX-CONVRT.
112700     MOVE HEXDEC-CONVRT TO CHAR-CONVRT.
112800     GO TO EXIT-CONVRT.
112900 DEC-TO-HEX-CONVRT.
113000     IF DEC-CONVRT > 256
113100         MOVE "04"     TO RTC-CONVRT
113200         GO TO EXIT-CONVRT.
113300     ADD 1  DEC-CONVRT GIVING SUBHX-CONVRT.
113400     MOVE HEX-VALUE-CONVRT (SUBHX-CONVRT) TO HEX-CONVRT.
113500     GO TO EXIT-CONVRT.
113600 HEX-TO-CHR-STR-CONVRT.
113700     IF LEN-CONVRT = 00 OR > 80
113800         MOVE "05"     TO RTC-CONVRT
113900         GO TO EXIT-CONVRT.
114000     MOVE  INP-CONVRT  TO AREA-I-CONVRT.
114100     MOVE    SPACES    TO AREA-O-CONVRT.
114200     PERFORM HEX-CHARACTER-CONVRT THRU HEX-CHAR-CONVRT
114300         VARYING SUBBT-CONVRT FROM 1 BY 1 UNTIL
114400           SUBBT-CONVRT > LEN-CONVRT OR RTC-CONVRT NOT = "00".
114500     MOVE AREA-O-CONVRT TO OUT-CONVRT.
114600     GO TO EXIT-CONVRT.
114700 HEX-TO-CHR-CONVRT.
114800     SET TBLES-CONVRT  TO +1.
114900     MOVE    ZEROES    TO SUBHX-CONVRT.
115000     SEARCH HEX-VALUE-CONVRT VARYING SUBHX-CONVRT
115100         AT END MOVE "03" TO RTC-CONVRT
115200           WHEN HEX-VALUE-CONVRT (SUBHX-CONVRT) = HEX-CONVRT
115300                SUBTRACT 1 FROM SUBHX-CONVRT
115400                MOVE SUBHX-CONVRT TO DECHEX-CONVRT
115500                MOVE HEXDEC-CONVRT TO CHAR-CONVRT.
115600     GO TO EXIT-CONVRT.
115700 HEX-TO-DEC-CONVRT.
115800     SET TBLES-CONVRT  TO +1.
115900     MOVE    ZEROES    TO SUBHX-CONVRT.
116000     SEARCH HEX-VALUE-CONVRT VARYING SUBHX-CONVRT
116100         AT END MOVE "03" TO RTC-CONVRT
116200           WHEN HEX-VALUE-CONVRT (SUBHX-CONVRT) = HEX-CONVRT
116300                SUBTRACT 1 FROM SUBHX-CONVRT
116400                MOVE SUBHX-CONVRT TO DEC-CONVRT.
116500     GO TO EXIT-CONVRT.
116600 CHR-TO-HEX-STR-CONVRT.
116700     IF LEN-CONVRT = 00 OR > 80
116800         MOVE "05"     TO RTC-CONVRT
116900         GO TO EXIT-CONVRT.
117000     MOVE  INP-CONVRT  TO AREA-I-CONVRT.
117100     MOVE    SPACES    TO AREA-O-CONVRT.
117200     PERFORM CHARACTER-HEX-CONVRT THRU CHAR-HEX-CONVRT
117300         VARYING SUBBT-CONVRT FROM 1 BY 1 UNTIL
117400           SUBBT-CONVRT > LEN-CONVRT.
117500     MOVE AREA-O-CONVRT TO OUT-CONVRT.
117600     GO TO EXIT-CONVRT.
117700 CHR-TO-HEX-CONVRT.
117800     MOVE CHAR-CONVRT  TO HEXDEC-CONVRT.
117900     ADD 1 DECHEX-CONVRT GIVING SUBHX-CONVRT.
118000     MOVE HEX-VALUE-CONVRT (SUBHX-CONVRT) TO HEX-CONVRT.
118100     GO TO EXIT-CONVRT.
118200 CHR-TO-DEC-CONVRT.
118300     MOVE CHAR-CONVRT  TO HEXDEC-CONVRT.
118400     MOVE DECHEX-CONVRT TO DEC-CONVRT.
118500     GO TO EXIT-CONVRT.
118600 CHARACTER-HEX-CONVRT.
118700     MOVE BYTI-1-CONVRT (SUBBT-CONVRT) TO HEXDEC-CONVRT.
118800     ADD 1 DECHEX-CONVRT GIVING SUBHX-CONVRT.
118900     MOVE HEX-VALUE-CONVRT (SUBHX-CONVRT)
119000          TO BYTO-2-CONVRT (SUBBT-CONVRT).
119100 CHAR-HEX-CONVRT.
119200     EXIT.
119300 HEX-CHARACTER-CONVRT.
119400     SET TBLES-CONVRT  TO +1.
119500     MOVE    ZEROES    TO SUBHX-CONVRT.
119600     SEARCH HEX-VALUE-CONVRT VARYING SUBHX-CONVRT
119700         AT END MOVE "03" TO RTC-CONVRT
119800                GO TO HEX-CHAR-CONVRT
119900           WHEN HEX-VALUE-CONVRT (SUBHX-CONVRT) =
120000                   BYTI-2-CONVRT (SUBBT-CONVRT)
120100                NEXT SENTENCE.
120200     SUBTRACT 1 FROM SUBHX-CONVRT.
120300     MOVE SUBHX-CONVRT TO DECHEX-CONVRT.
120400     MOVE HEXDEC-CONVRT TO BYTO-1-CONVRT (SUBBT-CONVRT).
120500 HEX-CHAR-CONVRT.
120600     EXIT.
120700 ADDRESS-DECM-CONVRT.
120800     MOVE    ZEROES    TO MULT-CONVRT.
120900     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "0" GO TO ADDR-DECM-CONVRT.
121000     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "1" MOVE 01 TO MULT-CONVRT.
121100     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "2" MOVE 02 TO MULT-CONVRT.
121200     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "3" MOVE 03 TO MULT-CONVRT.
121300     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "4" MOVE 04 TO MULT-CONVRT.
121400     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "5" MOVE 05 TO MULT-CONVRT.
121500     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "6" MOVE 06 TO MULT-CONVRT.
121600     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "7" MOVE 07 TO MULT-CONVRT.
121700     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "8" MOVE 08 TO MULT-CONVRT.
121800     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "9" MOVE 09 TO MULT-CONVRT.
121900     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "A" MOVE 10 TO MULT-CONVRT.
122000     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "B" MOVE 11 TO MULT-CONVRT.
122100     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "C" MOVE 12 TO MULT-CONVRT.
122200     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "D" MOVE 13 TO MULT-CONVRT.
122300     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "E" MOVE 14 TO MULT-CONVRT.
122400     IF BYTI-1-CONVRT (SUBBT-CONVRT) = "F" MOVE 15 TO MULT-CONVRT.
122500     IF MULT-CONVRT = ZERO
122600         MOVE   "03"   TO RTC-CONVRT
122700         GO TO ADDR-DECM-CONVRT.
122800     COMPUTE DEC-CONVRT = DEC-CONVRT
122900                 + ADR-VALUE-CONVRT (SUBBT-CONVRT) * MULT-CONVRT.
123000 ADDR-DECM-CONVRT.
123100     EXIT.
123200 DECM-ADDRESS-CONVRT.
123300     DIVIDE DEC-CONVRT BY ADR-VALUE-CONVRT (SUBBT-CONVRT)
123400         GIVING MULT-CONVRT REMAINDER DEC-CONVRT.
123500     IF MULT-CONVRT > 15
123600         MOVE   "04"   TO RTC-CONVRT
123700         GO TO DECM-ADDR-CONVRT.
123800     IF MULT-CONVRT = 00 MOVE "0" TO BYTI-1-CONVRT (SUBBT-CONVRT).
123900     IF MULT-CONVRT = 01 MOVE "1" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124000     IF MULT-CONVRT = 02 MOVE "2" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124100     IF MULT-CONVRT = 03 MOVE "3" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124200     IF MULT-CONVRT = 04 MOVE "4" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124300     IF MULT-CONVRT = 05 MOVE "5" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124400     IF MULT-CONVRT = 06 MOVE "6" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124500     IF MULT-CONVRT = 07 MOVE "7" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124600     IF MULT-CONVRT = 08 MOVE "8" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124700     IF MULT-CONVRT = 09 MOVE "9" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124800     IF MULT-CONVRT = 10 MOVE "A" TO BYTI-1-CONVRT (SUBBT-CONVRT).
124900     IF MULT-CONVRT = 11 MOVE "B" TO BYTI-1-CONVRT (SUBBT-CONVRT).
125000     IF MULT-CONVRT = 12 MOVE "C" TO BYTI-1-CONVRT (SUBBT-CONVRT).
125100     IF MULT-CONVRT = 13 MOVE "D" TO BYTI-1-CONVRT (SUBBT-CONVRT).
125200     IF MULT-CONVRT = 14 MOVE "E" TO BYTI-1-CONVRT (SUBBT-CONVRT).
125300     IF MULT-CONVRT = 15 MOVE "F" TO BYTI-1-CONVRT (SUBBT-CONVRT).
125400 DECM-ADDR-CONVRT.
125500     EXIT.
125600 EXIT-CONVRT.
125700     MOVE    ZEROES    TO SUBBT-CONVRT.
125800*
125900 X10-CONTINUE.
126000     MOVE "UNDEFINED ERROR HAS OCCURED - RETURN CODE ==> "
126100                       TO OUT-MES.
126200     MOVE  OUT-CONVRT  TO OUT-RTC.
126300     MOVE    +0050     TO I-O-LEN.
126400*
126500     EXEC CICS SEND FROM(OUT-MSG)
126600                    LENGTH(I-O-LEN)
126700                    CTLCHAR(CTLCHR47)
126800                    ERASE
126900                    END-EXEC.
127000     EXEC CICS DELETEQ TS QUEUE(QUEUE-NAME)
127100                          NOHANDLE
127200                          END-EXEC.
127300     EXEC CICS RETURN END-EXEC.
127400 EJECT
127500******************************************************************
127600*                                                                *
127700*    TRANSACTION EXIT ROUTINE         (BRANCH AT END OF GAME)    *
127800*                                                                *
127900******************************************************************
128000 Y00-END-OF-GAME.
128100     EXEC CICS SEND MAP(DATAMAP)
128200                    FROM(DATAMAPO)
128300                    MAPSET(OTHEMAP)
128400                    TERMINAL
128500                    FREEKB
128600                    FRSET
128700                    ALARM
128800                    END-EXEC.
128900     IF PIECES1I < PIECES2I
129000         MOVE "***** SORRY, BUT YOU LOST THE GAME!! *****"
129100             TO MESSAGO-MSG
129200         EXEC CICS SEND FROM(MESSAGO)
129300                        LENGTH(MESSAGL)
129400                        CTLCHAR(CTLCHR46)
129500                        END-EXEC
129600     ELSE
129700         IF PIECES1I > PIECES2I
129800             MOVE "***** HURRAY!!!  YOU WON THE GAME!!! *****"
129900                 TO MESSAGO-MSG
130000             EXEC CICS SEND FROM(MESSAGO)
130100                            LENGTH(MESSAGL)
130200                            CTLCHAR(CTLCHR46)
130300                            END-EXEC
130400         ELSE
130500             MOVE "***** THIS GAME HAS ENDED IN A TIE!! *****"
130600                 TO MESSAGO-MSG
130700             EXEC CICS SEND FROM(MESSAGO)
130800                            LENGTH(MESSAGL)
130900                            CTLCHAR(CTLCHR46)
131000                            END-EXEC.
131100     IF MOVEIS2I = "GO"
131200         EXEC CICS RECEIVE END-EXEC
131300         IF EIBAID = DFHPF1
131400             SUBTRACT 1 FROM INTERVAL-MOVENO
131500             EXEC CICS READQ TS QUEUE(QUEUE-NAME)
131600                                INTO(PLAYING-BOARD)
131700                                LENGTH(QUEUE-RLEN)
131800                                ITEM(INTERVAL-MOVENO)
131900                                END-EXEC
132000             MOVE "BW" TO MOVEIS1O
132100             MOVE LOW-VALUES TO INTERVAL-MVMODE
132200             MOVE ZEROES TO PIECES1O PIECES2O
132300             IF PLAYR = 1
132400                 INSPECT PLAYING-BOARD
132500                     TALLYING PIECES1O FOR ALL "X"
132600                 INSPECT PLAYING-BOARD
132700                     TALLYING PIECES2O FOR ALL "O"
132800                 GO TO B90-START-TRANSACTION
132900             ELSE
133000                 INSPECT PLAYING-BOARD
133100                     TALLYING PIECES1O FOR ALL "O"
133200                 INSPECT PLAYING-BOARD
133300                     TALLYING PIECES2O FOR ALL "X"
133400                 GO TO B90-START-TRANSACTION
133500         ELSE
133600             MOVE SPACES TO OUT-MSG
133700             MOVE +0001 TO I-O-LEN
133800             EXEC CICS SEND FROM(OUT-MSG)
133900                            LENGTH(I-O-LEN)
134000                            ERASE
134100                            END-EXEC
134200             EXEC CICS DELETEQ TS QUEUE(QUEUE-NAME)
134300                                  NOHANDLE
134400                                  END-EXEC.
134500     EXEC CICS RETURN END-EXEC.
134600 EJECT
134700******************************************************************
134800*                                                                *
134900*    TRANSACTION EXIT ROUTINE         (BRANCH ON ROUTE ERROR)    *
135000*                                                                *
135100******************************************************************
135200 Z00-EOJ.
135300     MOVE "INVALID TERMINAL ID SPECIFIED - RETRY - " TO OUT-MSG.
135400     MOVE    +0040     TO I-O-LEN.
135500*
135600     EXEC CICS SEND FROM(OUT-MSG)
135700                    LENGTH(I-O-LEN)
135800                    ERASE
135900                    END-EXEC.
136000     EXEC CICS RETURN END-EXEC.
136100*
136200     GOBACK.


