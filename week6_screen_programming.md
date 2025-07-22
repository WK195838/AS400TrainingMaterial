# 第六週：螢幕程式設計 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解AS/400螢幕程式設計的架構和原理
- 掌握DSPF（Display File）的設計和實作技術
- 熟練使用各種螢幕控制和格式化技巧
- 設計友善的使用者介面和互動流程
- 實作複雜的資料輸入驗證和錯誤處理機制
- 開發專業級的商業應用螢幕程式

---

## 🖥️ 第一節：螢幕程式設計概論

### 1.1 AS/400螢幕程式架構

#### 1.1.1 螢幕程式組成要素

```
AS/400 螢幕程式系統架構：
┌─────────────────────────────────────────┐
│ 使用者介面層 (User Interface Layer)     │
│ ┌─────────────────────────────────────┐ │
│ │ 5250 終端機顯示                    │ │
│ │ - 24行x80列標準顯示                │ │
│ │ - 功能鍵支援 (F1-F24)              │ │
│ │ - 游標控制和欄位保護               │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 資料交換
┌─────────────────────────────────────────┐
│ 顯示檔案層 (Display File Layer - DSPF) │
│ ┌─────────────────────────────────────┐ │
│ │ DDS定義螢幕格式                    │ │
│ │ - 欄位定義和屬性                   │ │
│ │ - 功能鍵對應                       │ │
│ │ - 錯誤訊息處理                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 程式控制
┌─────────────────────────────────────────┐
│ 應用程式層 (Application Program Layer) │
│ ┌─────────────────────────────────────┐ │
│ │ COBOL應用程式                      │ │
│ │ - 商業邏輯處理                     │ │
│ │ - 資料驗證                         │ │
│ │ - 檔案存取控制                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

#### 1.1.2 螢幕程式設計原則

```cobol
      *    螢幕程式設計的最佳實務原則：
      *
      *    1. 使用者體驗 (User Experience)
      *       - 直觀的畫面配置
      *       - 一致的操作模式
      *       - 清晰的錯誤訊息
      *       - 適當的預設值
      *
      *    2. 效能考量 (Performance)
      *       - 最少的螢幕更新次數
      *       - 有效的資料緩存
      *       - 適當的資料分頁
      *
      *    3. 維護性 (Maintainability)
      *       - 模組化的螢幕設計
      *       - 標準化的命名規則
      *       - 完整的註解文件
      *
      *    4. 安全性 (Security)
      *       - 適當的欄位保護
      *       - 敏感資料隱藏
      *       - 存取權限控制
```

### 1.2 顯示檔案基本概念

#### 1.2.1 記錄格式類型

```dds
      *    AS/400 顯示檔案記錄格式類型
      *
      *    1. 螢幕格式 (Screen Format)
      *       - 定義螢幕佈局和欄位
      *       - 控制使用者輸入和顯示
      *
      *    2. 子檔案格式 (Subfile Format)
      *       - 顯示多筆記錄的清單
      *       - 支援捲動和選取功能
      *
      *    3. 子檔案控制格式 (Subfile Control Format)
      *       - 控制子檔案的行為
      *       - 處理子檔案相關的功能鍵
      *
      *    4. 視窗格式 (Window Format)
      *       - 彈出式視窗顯示
      *       - 疊加在主畫面上
```

#### 1.2.2 DDS關鍵字分類

```dds
      * 顯示檔案DDS關鍵字分類
      *
      * 檔案層級關鍵字：
      * - DSPSIZ(24 80 *DS3)    螢幕大小
      * - PRINT                 支援列印
      * - INDARA                指示變數陣列
      * - CF01-CF24             功能鍵定義
      * - HELP                  說明功能
      *
      * 記錄層級關鍵字：
      * - OVERLAY               疊加顯示
      * - ERASE                 清除螢幕
      * - PUTOVR                強制輸出
      * - PUTRETAIN             保持顯示
      *
      * 欄位層級關鍵字：
      * - DSPATR(BL/UL/HI/RI)  顯示屬性
      * - PROTECT               保護欄位
      * - ERRMSG                錯誤訊息
      * - VALUES                有效值
      * - RANGE                 數值範圍
```

---

## 📝 第二節：基本螢幕格式設計

### 2.1 簡單螢幕格式

#### 2.1.1 員工查詢螢幕DDS

```dds
      *%%TS SD 20240125 100000 USER1    REL-V7R3M0 5770-WDS
      *%%EC
      * 
      * 員工查詢螢幕格式定義
      *
     A*%%TS SD 20240125 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      DSPSIZ(24 80 *DS3)
     A                                      CA03(03 'Exit')
     A                                      CA12(12 'Cancel')
     A                                      PRINT
     A                                      INDARA
     A            R SFL1                     
     A*%%TS SD 20240125 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      ERASE
     A                                      DSPATR(HI)
     A                                 1  2'EMPINQ'
     A                                 1 71DATE
     A                                 1 71EDTCDE(Y)
     A                                 2 71TIME
     A                                 1 30'員工查詢系統'
     A                                 1 30DSPATR(HI UL)
     A                                 4  2'員工編號  . . .'
     A            EMPID          6A  B  4 18DSPATR(UL)
     A                                 6  2'姓名  . . . . .'
     A            EMPNAME       20A  O  6 18DSPATR(HI)
     A                                 7  2'部門  . . . . .'
     A            DEPTNAME      20A  O  7 18DSPATR(HI)
     A                                 8  2'職位  . . . . .'
     A            POSITION      15A  O  8 18DSPATR(HI)
     A                                 9  2'薪資  . . . . .'
     A            SALARY         7Y 2O  9 18DSPATR(HI)
     A                                 9 18EDTCDE(1)
     A                                10  2'到職日期  . . .'
     A            HIREDATE      10A  O 10 18DSPATR(HI)
     A                                12  2'狀態  . . . . .'
     A            STATUS         1A  O 12 18DSPATR(HI)
     A                                23  2'F3=Exit   F12=Cancel'
     A                                23  2DSPATR(BL)
     A            R MSGSFL                   
     A*%%TS SD 20240125 100000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                24  2DSPATR(BL)
     A            MSGTEXT       78A  O 24  2DSPATR(BL)
```

#### 2.1.2 對應的COBOL程式

```cobol
      *****************************************************************
      * 程式名稱：EMPINQ                                             *
      * 程式功能：員工查詢程式                                       *
      * 作者：程式設計師                                             *
      * 日期：2024/01/25                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPINQ.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPINQD ASSIGN TO WORKSTN-EMPINQD.
           
           SELECT EMPLOYEE-FILE
               ASSIGN TO DATABASE-EMPMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EMP-ID
               FILE STATUS IS WS-EMP-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  EMPINQD
           USAGE IS DISPLAY.
       01  EMPINQD-RECORD.
           COPY DDS-ALL-FORMATS OF EMPINQD.
           
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           COPY EMPFMT OF EMPMASTER.
           
       WORKING-STORAGE SECTION.
       
      *    螢幕控制變數
       01  WS-SCREEN-CONTROL.
           05  WS-FUNCTION-KEY     PIC X(2).
           05  WS-CONTINUE-PROGRAM PIC X.
               88  CONTINUE-PROGRAM        VALUE 'Y'.
               88  EXIT-PROGRAM            VALUE 'N'.
           05  WS-SCREEN-MODE      PIC X(10).
               88  INQUIRY-MODE            VALUE 'INQUIRY'.
               88  DISPLAY-MODE            VALUE 'DISPLAY'.
               
      *    檔案狀態
       01  WS-FILE-STATUS.
           05  WS-EMP-FILE-STATUS  PIC XX.
               88  EMP-FILE-SUCCESS        VALUE '00'.
               88  EMP-FILE-NOT-FOUND      VALUE '23'.
               
      *    工作變數
       01  WS-WORK-FIELDS.
           05  WS-SEARCH-EMPID     PIC X(6).
           05  WS-MESSAGE-TEXT     PIC X(78).
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM PROCESS-INQUIRY UNTIL EXIT-PROGRAM
           PERFORM TERMINATE-PROGRAM.
           STOP RUN.
           
       INITIALIZE-PROGRAM.
           OPEN I-O EMPINQD.
           OPEN INPUT EMPLOYEE-FILE.
           SET CONTINUE-PROGRAM TO TRUE.
           SET INQUIRY-MODE TO TRUE.
           MOVE SPACES TO WS-MESSAGE-TEXT.
           
       PROCESS-INQUIRY.
           PERFORM CLEAR-SCREEN-FIELDS
           PERFORM DISPLAY-INQUIRY-SCREEN
           PERFORM EVALUATE-FUNCTION-KEY.
           
       CLEAR-SCREEN-FIELDS.
           MOVE SPACES TO EMPID OF SFL1.
           MOVE SPACES TO EMPNAME OF SFL1.
           MOVE SPACES TO DEPTNAME OF SFL1.
           MOVE SPACES TO POSITION OF SFL1.
           MOVE ZERO TO SALARY OF SFL1.
           MOVE SPACES TO HIREDATE OF SFL1.
           MOVE SPACES TO STATUS OF SFL1.
           MOVE SPACES TO MSGTEXT OF MSGSFL.
           
       DISPLAY-INQUIRY-SCREEN.
           IF INQUIRY-MODE
               WRITE EMPINQD-RECORD FORMAT IS SFL1
           END-IF.
           
           WRITE EMPINQD-RECORD FORMAT IS MSGSFL.
           READ EMPINQD-RECORD FORMAT IS SFL1.
           MOVE FUNCTION UPPER-CASE(EMPID OF SFL1) TO WS-SEARCH-EMPID.
           
       EVALUATE-FUNCTION-KEY.
           IF IN03 OR IN12
               SET EXIT-PROGRAM TO TRUE
           ELSE
               IF WS-SEARCH-EMPID NOT = SPACES
                   PERFORM SEARCH-EMPLOYEE
               ELSE
                   MOVE '請輸入員工編號' TO WS-MESSAGE-TEXT
                   PERFORM DISPLAY-MESSAGE
               END-IF
           END-IF.
           
       SEARCH-EMPLOYEE.
           MOVE WS-SEARCH-EMPID TO EMP-ID OF EMPLOYEE-RECORD.
           READ EMPLOYEE-FILE
               INVALID KEY
                   MOVE '員工編號不存在: ' TO WS-MESSAGE-TEXT
                   STRING WS-SEARCH-EMPID DELIMITED BY SPACE
                          INTO WS-MESSAGE-TEXT WITH POINTER 13
                   PERFORM DISPLAY-MESSAGE
               NOT INVALID KEY
                   PERFORM DISPLAY-EMPLOYEE-DATA
           END-READ.
           
       DISPLAY-EMPLOYEE-DATA.
           MOVE EMP-ID OF EMPLOYEE-RECORD TO EMPID OF SFL1.
           MOVE EMP-NAME OF EMPLOYEE-RECORD TO EMPNAME OF SFL1.
           
      *    取得部門名稱
           PERFORM GET-DEPARTMENT-NAME.
           
           MOVE EMP-POSITION OF EMPLOYEE-RECORD TO POSITION OF SFL1.
           MOVE EMP-SALARY OF EMPLOYEE-RECORD TO SALARY OF SFL1.
           MOVE EMP-HIRE-DATE OF EMPLOYEE-RECORD TO HIREDATE OF SFL1.
           MOVE EMP-STATUS OF EMPLOYEE-RECORD TO STATUS OF SFL1.
           
           MOVE '查詢成功' TO WS-MESSAGE-TEXT.
           PERFORM DISPLAY-MESSAGE.
           SET DISPLAY-MODE TO TRUE.
           
       GET-DEPARTMENT-NAME.
      *    這裡應該查詢部門檔案取得部門名稱
      *    為簡化範例，直接使用部門代碼對應
           EVALUATE EMP-DEPT OF EMPLOYEE-RECORD
               WHEN 'IT01'
                   MOVE '資訊技術部' TO DEPTNAME OF SFL1
               WHEN 'HR01'
                   MOVE '人力資源部' TO DEPTNAME OF SFL1
               WHEN 'FN01'
                   MOVE '財務部' TO DEPTNAME OF SFL1
               WHEN 'SL01'
                   MOVE '業務部' TO DEPTNAME OF SFL1
               WHEN OTHER
                   MOVE EMP-DEPT OF EMPLOYEE-RECORD TO DEPTNAME OF SFL1
           END-EVALUATE.
           
       DISPLAY-MESSAGE.
           MOVE WS-MESSAGE-TEXT TO MSGTEXT OF MSGSFL.
           
       TERMINATE-PROGRAM.
           CLOSE EMPINQD.
           CLOSE EMPLOYEE-FILE.
```

### 2.2 資料維護螢幕

#### 2.2.1 員工維護螢幕DDS

```dds
      *
      * 員工資料維護螢幕
      *
     A*%%TS SD 20240125 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      DSPSIZ(24 80 *DS3)
     A                                      CF03(03 'Exit')
     A                                      CF05(05 'Refresh')
     A                                      CF06(06 'Add')
     A                                      CF11(11 'Update')
     A                                      CF12(12 'Cancel')
     A                                      PRINT
     A                                      INDARA
     A            R MAINTSCR                 
     A*%%TS SD 20240125 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                      ERASE
     A                                 1  2'EMPMAINT'
     A                                 1 34'員工資料維護'
     A                                 1 34DSPATR(HI UL)
     A                                 1 71DATE
     A                                 1 71EDTCDE(Y)
     A                                 2 71TIME
     A                                 3  2'模式:'
     A            MODE           8A  O  3  8DSPATR(HI)
     A                                 5  2'員工編號  . . .'
     A            EMPID          6A  B  5 18DSPATR(UL)
     A N32                             5 18PROTECT
     A                                 6  2'姓名  . . . . .'
     A            EMPNAME       20A  B  6 18DSPATR(UL)
     A                                 6 18ERRMSG(31 '姓名不可空白')
     A                                 7  2'部門代碼  . . .'
     A            DEPTCODE       4A  B  7 18DSPATR(UL)
     A                                 7 18VALUES('IT01' 'HR01' 'FN01' 'SL01')
     A                                 7 18ERRMSG(32 '無效的部門代碼')
     A                                 7 25'IT01=資訊部 HR01=人資部'
     A                                 8 25'FN01=財務部 SL01=業務部'
     A                                 9  2'職位  . . . . .'
     A            POSITION      15A  B  9 18DSPATR(UL)
     A                                10  2'薪資  . . . . .'
     A            SALARY         7Y 2B 10 18DSPATR(UL)
     A                                10 18EDTCDE(1)
     A                                10 18RANGE(0 999999.99)
     A                                10 18ERRMSG(33 '薪資必須大於0')
     A                                11  2'到職日期  . . .'
     A            HIREDATE      10A  B 11 18DSPATR(UL)
     A                                11 18EDTCDE(Y)
     A                                11 18ERRMSG(34 '日期格式錯誤(YYYY-MM-DD)')
     A                                12  2'狀態  . . . . .'
     A            STATUS         1A  B 12 18DSPATR(UL)
     A                                12 18VALUES('A' 'I' 'R')
     A                                12 18ERRMSG(35 '狀態:A=在職 I=停職 R=離職')
     A                                12 22'A=在職 I=停職 R=離職'
     A                                14  2'電子郵件  . . .'
     A            EMAIL         30A  B 14 18DSPATR(UL)
     A                                15  2'電話  . . . . .'
     A            PHONE         15A  B 15 18DSPATR(UL)
     A                                17  2'主管編號  . . .'
     A            SUPERVISOR     6A  B 17 18DSPATR(UL)
     A                                19  2'建立者:'
     A            CREATEDBY     10A  O 19 12DSPATR(HI)
     A                                19 25'建立時間:'
     A            CREATEDTS     19A  O 19 37DSPATR(HI)
     A                                20  2'更新者:'
     A            UPDATEDBY     10A  O 20 12DSPATR(HI)
     A                                20 25'更新時間:'
     A            UPDATEDTS     19A  O 20 37DSPATR(HI)
     A                                22  2'F3=Exit  F5=Refresh  F6=Add'
     A                                22  2DSPATR(BL)
     A                                23  2'F11=Update  F12=Cancel'
     A                                23  2DSPATR(BL)
     A            R MSGLINE                  
     A*%%TS SD 20240125 110000 USER1    REL-V7R3M0 5770-WDS
     A*%%EC
     A                                24  2DSPATR(BL)
     A            MSGTEXT       78A  O 24  2DSPATR(BL)
```

#### 2.2.2 員工維護程式主邏輯

```cobol
      *****************************************************************
      * 程式名稱：EMPMAINT                                           *
      * 程式功能：員工資料維護程式                                   *
      * 作者：程式設計師                                             *
      * 日期：2024/01/25                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPMAINT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPMAINTD ASSIGN TO WORKSTN-EMPMAINTD.
           
           SELECT EMPLOYEE-FILE
               ASSIGN TO DATABASE-EMPMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EMP-ID
               FILE STATUS IS WS-EMP-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  EMPMAINTD
           USAGE IS DISPLAY.
       01  EMPMAINTD-RECORD.
           COPY DDS-ALL-FORMATS OF EMPMAINTD.
           
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           COPY EMPFMT OF EMPMASTER.
           
       WORKING-STORAGE SECTION.
       
      *    程式控制變數
       01  WS-PROGRAM-CONTROL.
           05  WS-PROGRAM-MODE     PIC X(10).
               88  INQUIRY-MODE            VALUE 'INQUIRY'.
               88  ADD-MODE                VALUE 'ADD'.
               88  UPDATE-MODE             VALUE 'UPDATE'.
               88  DISPLAY-MODE            VALUE 'DISPLAY'.
           05  WS-CONTINUE-PROGRAM PIC X.
               88  CONTINUE-PROGRAM        VALUE 'Y'.
               88  EXIT-PROGRAM            VALUE 'N'.
           05  WS-RECORD-FOUND     PIC X.
               88  RECORD-FOUND            VALUE 'Y'.
               88  RECORD-NOT-FOUND        VALUE 'N'.
               
      *    檔案狀態
       01  WS-FILE-STATUS.
           05  WS-EMP-FILE-STATUS  PIC XX.
               88  EMP-FILE-SUCCESS        VALUE '00'.
               88  EMP-FILE-NOT-FOUND      VALUE '23'.
               88  EMP-FILE-DUPLICATE      VALUE '22'.
               
      *    螢幕欄位備份
       01  WS-SCREEN-BACKUP.
           05  WS-BACKUP-EMPID     PIC X(6).
           05  WS-BACKUP-EMPNAME   PIC X(20).
           05  WS-BACKUP-DEPTCODE  PIC X(4).
           05  WS-BACKUP-POSITION  PIC X(15).
           05  WS-BACKUP-SALARY    PIC 9(7)V99.
           05  WS-BACKUP-HIREDATE  PIC X(10).
           05  WS-BACKUP-STATUS    PIC X.
           05  WS-BACKUP-EMAIL     PIC X(30).
           05  WS-BACKUP-PHONE     PIC X(15).
           05  WS-BACKUP-SUPERVISOR PIC X(6).
           
      *    工作變數
       01  WS-WORK-FIELDS.
           05  WS-MESSAGE-TEXT     PIC X(78).
           05  WS-CURRENT-USER     PIC X(10) VALUE 'SYSTEM'.
           05  WS-CURRENT-TS       PIC X(19).
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM MAIN-PROCESS UNTIL EXIT-PROGRAM
           PERFORM TERMINATE-PROGRAM.
           STOP RUN.
           
       INITIALIZE-PROGRAM.
           OPEN I-O EMPMAINTD.
           OPEN I-O EMPLOYEE-FILE.
           SET CONTINUE-PROGRAM TO TRUE.
           SET INQUIRY-MODE TO TRUE.
           MOVE SPACES TO WS-MESSAGE-TEXT.
           
       MAIN-PROCESS.
           PERFORM CLEAR-SCREEN-FIELDS
           PERFORM SETUP-SCREEN-MODE
           PERFORM DISPLAY-MAINTENANCE-SCREEN
           PERFORM PROCESS-FUNCTION-KEYS.
           
       CLEAR-SCREEN-FIELDS.
           MOVE SPACES TO EMPID OF MAINTSCR.
           MOVE SPACES TO EMPNAME OF MAINTSCR.
           MOVE SPACES TO DEPTCODE OF MAINTSCR.
           MOVE SPACES TO POSITION OF MAINTSCR.
           MOVE ZERO TO SALARY OF MAINTSCR.
           MOVE SPACES TO HIREDATE OF MAINTSCR.
           MOVE 'A' TO STATUS OF MAINTSCR.
           MOVE SPACES TO EMAIL OF MAINTSCR.
           MOVE SPACES TO PHONE OF MAINTSCR.
           MOVE SPACES TO SUPERVISOR OF MAINTSCR.
           MOVE SPACES TO CREATEDBY OF MAINTSCR.
           MOVE SPACES TO CREATEDTS OF MAINTSCR.
           MOVE SPACES TO UPDATEDBY OF MAINTSCR.
           MOVE SPACES TO UPDATEDTS OF MAINTSCR.
           
      *    清除錯誤指示器
           MOVE '0' TO IN31, IN32, IN33, IN34, IN35.
           
       SETUP-SCREEN-MODE.
           EVALUATE TRUE
               WHEN INQUIRY-MODE
                   MOVE 'INQUIRY' TO MODE OF MAINTSCR
                   MOVE '0' TO IN32  *允許編號輸入
               WHEN ADD-MODE
                   MOVE 'ADD' TO MODE OF MAINTSCR
                   MOVE '0' TO IN32  *允許編號輸入
               WHEN UPDATE-MODE
                   MOVE 'UPDATE' TO MODE OF MAINTSCR
                   MOVE '1' TO IN32  *保護編號欄位
               WHEN DISPLAY-MODE
                   MOVE 'DISPLAY' TO MODE OF MAINTSCR
                   MOVE '1' TO IN32  *保護編號欄位
           END-EVALUATE.
           
       DISPLAY-MAINTENANCE-SCREEN.
           WRITE EMPMAINTD-RECORD FORMAT IS MAINTSCR.
           WRITE EMPMAINTD-RECORD FORMAT IS MSGLINE.
           READ EMPMAINTD-RECORD FORMAT IS MAINTSCR.
           
       PROCESS-FUNCTION-KEYS.
           EVALUATE TRUE
               WHEN IN03
                   SET EXIT-PROGRAM TO TRUE
               WHEN IN05
                   PERFORM REFRESH-SCREEN
               WHEN IN06
                   PERFORM PREPARE-ADD-MODE
               WHEN IN11
                   PERFORM PROCESS-UPDATE
               WHEN IN12
                   PERFORM CANCEL-OPERATION
               WHEN OTHER
                   PERFORM PROCESS-INQUIRY
           END-EVALUATE.
           
       REFRESH-SCREEN.
           SET INQUIRY-MODE TO TRUE.
           MOVE SPACES TO WS-MESSAGE-TEXT.
           
       PREPARE-ADD-MODE.
           SET ADD-MODE TO TRUE.
           PERFORM CLEAR-SCREEN-FIELDS.
           MOVE '準備新增員工資料' TO WS-MESSAGE-TEXT.
           PERFORM DISPLAY-MESSAGE.
           
       PROCESS-INQUIRY.
           IF EMPID OF MAINTSCR NOT = SPACES
               PERFORM SEARCH-EMPLOYEE
           ELSE
               MOVE '請輸入員工編號' TO WS-MESSAGE-TEXT
               PERFORM DISPLAY-MESSAGE
           END-IF.
           
       SEARCH-EMPLOYEE.
           MOVE FUNCTION UPPER-CASE(EMPID OF MAINTSCR) 
               TO EMP-ID OF EMPLOYEE-RECORD.
           
           READ EMPLOYEE-FILE
               INVALID KEY
                   SET RECORD-NOT-FOUND TO TRUE
                   MOVE '員工編號不存在' TO WS-MESSAGE-TEXT
                   PERFORM DISPLAY-MESSAGE
               NOT INVALID KEY
                   SET RECORD-FOUND TO TRUE
                   PERFORM LOAD-EMPLOYEE-DATA
                   SET UPDATE-MODE TO TRUE
                   MOVE '資料載入完成，可進行修改' TO WS-MESSAGE-TEXT
                   PERFORM DISPLAY-MESSAGE
           END-READ.
           
       LOAD-EMPLOYEE-DATA.
           MOVE EMP-ID OF EMPLOYEE-RECORD TO EMPID OF MAINTSCR.
           MOVE EMP-NAME OF EMPLOYEE-RECORD TO EMPNAME OF MAINTSCR.
           MOVE EMP-DEPT OF EMPLOYEE-RECORD TO DEPTCODE OF MAINTSCR.
           MOVE EMP-POSITION OF EMPLOYEE-RECORD TO POSITION OF MAINTSCR.
           