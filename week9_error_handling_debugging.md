# 第九週：錯誤處理與除錯 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解AS/400系統的錯誤處理機制和異常管理
- 掌握STRDBG除錯器的使用技巧和進階功能
- 設計健全的錯誤處理策略和復原機制
- 建立完整的日誌記錄和監控系統
- 進行系統效能分析和調校
- 開發高品質、可維護的專業程式碼

---

## 🚨 第一節：錯誤處理機制

### 1.1 AS/400錯誤類型分類

#### 1.1.1 系統層級錯誤

```cobol
      *    AS/400 系統錯誤分類架構：
      *
      *    1. 系統層級錯誤 (System Level Errors)
      *       - 硬體故障錯誤
      *       - 作業系統錯誤
      *       - 記憶體不足
      *       - 系統資源耗盡
      *
      *    2. 程式層級錯誤 (Program Level Errors)
      *       - 語法錯誤 (編譯時期)
      *       - 執行時期錯誤
      *       - 邏輯錯誤
      *       - 資料型態錯誤
      *
      *    3. 資料層級錯誤 (Data Level Errors)
      *       - 檔案存取錯誤
      *       - 記錄鎖定錯誤
      *       - 資料完整性錯誤
      *       - SQL執行錯誤
      *
      *    4. 應用層級錯誤 (Application Level Errors)
      *       - 商業規則違反
      *       - 使用者輸入錯誤
      *       - 外部服務錯誤
      *       - 整合介面錯誤
```

#### 1.1.2 錯誤處理策略

```
錯誤處理策略層次：
┌─────────────────────────────────────────┐
│ 預防性處理 (Preventive Handling)        │
│ ┌─────────────────────────────────────┐ │
│ │ - 輸入驗證和資料檢查               │ │
│ │ - 前置條件確認                     │ │
│ │ - 資源可用性檢查                   │ │
│ │ - 參數合法性驗證                   │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓ 錯誤發生
┌─────────────────────────────────────────┐
│ 偵測與捕捉 (Detection & Catching)       │
│ ┌─────────────────────────────────────┐ │
│ │ - 異常事件監控                     │ │
│ │ - 錯誤狀態檢查                     │ │
│ │ - 自動偵測機制                     │ │
│ │ - 即時警報系統                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓ 錯誤處理
┌─────────────────────────────────────────┐
│ 回復與修正 (Recovery & Correction)      │
│ ┌─────────────────────────────────────┐ │
│ │ - 自動修復機制                     │ │
│ │ - 資料回復程序                     │ │
│ │ - 替代方案執行                     │ │
│ │ - 優雅降級處理                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↓ 事後處理
┌─────────────────────────────────────────┐
│ 記錄與報告 (Logging & Reporting)        │
│ ┌─────────────────────────────────────┐ │
│ │ - 完整錯誤日誌                     │ │
│ │ - 事件追蹤記錄                     │ │
│ │ - 統計分析報告                     │ │
│ │ - 改善建議提供                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

### 1.2 檔案操作錯誤處理

#### 1.2.1 完整的檔案錯誤處理框架

```cobol
      *****************************************************************
      * 程式名稱：FILEERR                                            *
      * 程式功能：檔案操作錯誤處理示範                               *
      * 作者：錯誤處理專家                                           *
      * 日期：2024/02/10                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEERR.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO DATABASE-EMPMASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EMP-ID
               FILE STATUS IS WS-EMP-FILE-STATUS.
               
           SELECT ERROR-LOG-FILE
               ASSIGN TO DATABASE-ERRORLOG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LOG-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           COPY EMPFMT OF EMPMASTER.
           
       FD  ERROR-LOG-FILE.
       01  ERROR-LOG-RECORD.
           05  LOG-TIMESTAMP       PIC X(26).
           05  LOG-PROGRAM-ID      PIC X(10).
           05  LOG-OPERATION       PIC X(10).
           05  LOG-FILE-NAME       PIC X(10).
           05  LOG-RECORD-KEY      PIC X(20).
           05  LOG-FILE-STATUS     PIC XX.
           05  LOG-ERROR-MESSAGE   PIC X(100).
           05  LOG-USER-ID         PIC X(10).
           05  LOG-JOB-INFO        PIC X(28).
           
       WORKING-STORAGE SECTION.
       
      *    檔案狀態控制
       01  WS-FILE-STATUS-CONTROL.
           05  WS-EMP-FILE-STATUS  PIC XX.
               88  EMP-SUCCESS             VALUE '00'.
               88  EMP-END-OF-FILE         VALUE '10'.
               88  EMP-RECORD-NOT-FOUND    VALUE '23'.
               88  EMP-DUPLICATE-KEY       VALUE '22'.
               88  EMP-RECORD-LOCKED       VALUE '9D'.
               88  EMP-FILE-NOT-OPEN       VALUE '48'.
               88  EMP-INVALID-KEY         VALUE '24'.
               88  EMP-BOUNDARY-ERROR      VALUE '34'.
               88  EMP-FILE-DAMAGED        VALUE '30'.
               88  EMP-NO-SPACE            VALUE '37'.
               88  EMP-PERMISSION-ERROR    VALUE '38'.
               88  EMP-FATAL-ERROR         VALUE '90' THRU '99'.
               
           05  WS-LOG-FILE-STATUS  PIC XX.
               88  LOG-SUCCESS             VALUE '00'.
               
      *    錯誤處理控制
       01  WS-ERROR-CONTROL.
           05  WS-ERROR-OCCURRED   PIC X.
               88  ERROR-OCCURRED          VALUE 'Y'.
               88  NO-ERROR                VALUE 'N'.
           05  WS-RETRY-COUNT      PIC 9(2) VALUE 0.
           05  WS-MAX-RETRIES      PIC 9(2) VALUE 3.
           05  WS-OPERATION-TYPE   PIC X(10).
           05  WS-TARGET-KEY       PIC X(20).
           
      *    錯誤訊息定義
       01  WS-ERROR-MESSAGES.
           05  WS-MSG-TABLE.
               10  WS-MSG-ENTRY OCCURS 20 TIMES.
                   15  WS-MSG-STATUS   PIC XX.
                   15  WS-MSG-TEXT     PIC X(80).
                   
      *    系統資訊
       01  WS-SYSTEM-INFO.
           05  WS-CURRENT-USER     PIC X(10).
           05  WS-CURRENT-JOB      PIC X(28).
           05  WS-CURRENT-TIMESTAMP PIC X(26).
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-ERROR-HANDLING
           PERFORM DEMONSTRATE-FILE-OPERATIONS
           PERFORM CLEANUP-AND-EXIT.
           STOP RUN.
           
       INITIALIZE-ERROR-HANDLING.
           DISPLAY 'Initializing Error Handling Framework...'.
           
      *    載入錯誤訊息表
           PERFORM LOAD-ERROR-MESSAGES.
           
      *    取得系統資訊
           PERFORM GET-SYSTEM-INFORMATION.
           
      *    開啟錯誤日誌檔
           PERFORM OPEN-ERROR-LOG.
           
           DISPLAY 'Error handling framework initialized.'.
           
       LOAD-ERROR-MESSAGES.
      *    建立錯誤代碼對應訊息表
           MOVE '00' TO WS-MSG-STATUS(1).
           MOVE 'Operation completed successfully' TO WS-MSG-TEXT(1).
           
           MOVE '10' TO WS-MSG-STATUS(2).
           MOVE 'End of file reached' TO WS-MSG-TEXT(2).
           
           MOVE '22' TO WS-MSG-STATUS(3).
           MOVE 'Duplicate key error - record already exists' TO WS-MSG-TEXT(3).
           
           MOVE '23' TO WS-MSG-STATUS(4).
           MOVE 'Record not found for specified key' TO WS-MSG-TEXT(4).
           
           MOVE '24' TO WS-MSG-STATUS(5).
           MOVE 'Invalid key format or value' TO WS-MSG-TEXT(5).
           
           MOVE '30' TO WS-MSG-STATUS(6).
           MOVE 'File damaged or corrupted' TO WS-MSG-TEXT(6).
           
           MOVE '34' TO WS-MSG-STATUS(7).
           MOVE 'Boundary violation or record too large' TO WS-MSG-TEXT(7).
           
           MOVE '37' TO WS-MSG-STATUS(8).
           MOVE 'No space available for operation' TO WS-MSG-TEXT(8).
           
           MOVE '38' TO WS-MSG-STATUS(9).
           MOVE 'Permission denied or file locked' TO WS-MSG-TEXT(9).
           
           MOVE '48' TO WS-MSG-STATUS(10).
           MOVE 'File not open or invalid file handle' TO WS-MSG-TEXT(10).
           
           MOVE '9D' TO WS-MSG-STATUS(11).
           MOVE 'Record locked by another process' TO WS-MSG-TEXT(11).
           
       GET-SYSTEM-INFORMATION.
      *    使用系統API取得當前使用者和作業資訊
           CALL 'QWCRSVAL' USING
               'QCURRENT_USER'
               WS-CURRENT-USER.
               
           CALL 'QWCRSVAL' USING
               'QJOB'
               WS-CURRENT-JOB.
               
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP.
           
       OPEN-ERROR-LOG.
           OPEN EXTEND ERROR-LOG-FILE.
           IF NOT LOG-SUCCESS
               DISPLAY 'WARNING: Cannot open error log file'
               DISPLAY 'Status: ' WS-LOG-FILE-STATUS
           END-IF.
           
       DEMONSTRATE-FILE-OPERATIONS.
           DISPLAY 'Demonstrating File Operations with Error Handling...'.
           
      *    安全檔案開啟
           PERFORM SAFE-OPEN-EMPLOYEE-FILE.
           
           IF NO-ERROR
      *        各種檔案操作示範
               PERFORM SAFE-READ-OPERATION
               PERFORM SAFE-WRITE-OPERATION
               PERFORM SAFE-UPDATE-OPERATION
               PERFORM SAFE-DELETE-OPERATION
               
      *        關閉檔案
               PERFORM SAFE-CLOSE-EMPLOYEE-FILE
           END-IF.
           
       SAFE-OPEN-EMPLOYEE-FILE.
           MOVE 'OPEN' TO WS-OPERATION-TYPE.
           SET NO-ERROR TO TRUE.
           MOVE 0 TO WS-RETRY-COUNT.
           
           PERFORM UNTIL ERROR-OCCURRED OR WS-RETRY-COUNT >= WS-MAX-RETRIES
               OPEN I-O EMPLOYEE-FILE
               
               EVALUATE TRUE
                   WHEN EMP-SUCCESS
                       DISPLAY 'Employee file opened successfully'
                       EXIT PERFORM
                   WHEN EMP-FILE-NOT-OPEN OR EMP-PERMISSION-ERROR
                       ADD 1 TO WS-RETRY-COUNT
                       DISPLAY 'Retry opening file (' WS-RETRY-COUNT ')'
                       CALL 'QSLEEP' USING 1000  *等待1秒
                   WHEN OTHER
                       SET ERROR-OCCURRED TO TRUE
                       PERFORM LOG-FILE-ERROR
               END-EVALUATE
           END-PERFORM.
           
           IF WS-RETRY-COUNT >= WS-MAX-RETRIES
               SET ERROR-OCCURRED TO TRUE
               MOVE 'Max retries exceeded for file open' TO LOG-ERROR-MESSAGE
               PERFORM LOG-FILE-ERROR
           END-IF.
           
       SAFE-READ-OPERATION.
           DISPLAY 'Testing safe read operations...'.
           
      *    測試存在的記錄
           MOVE 'READ' TO WS-OPERATION-TYPE.
           MOVE 'E00001' TO EMP-ID.
           MOVE EMP-ID TO WS-TARGET-KEY.
           
           READ EMPLOYEE-FILE
               INVALID KEY
                   EVALUATE TRUE
                       WHEN EMP-RECORD-NOT-FOUND
                           DISPLAY 'Record not found: ' EMP-ID
                           PERFORM LOG-FILE-ERROR
                       WHEN EMP-RECORD-LOCKED
                           DISPLAY 'Record locked: ' EMP-ID
                           PERFORM HANDLE-RECORD-LOCK
                       WHEN OTHER
                           DISPLAY 'Read error: ' WS-EMP-FILE-STATUS
                           PERFORM LOG-FILE-ERROR
                   END-EVALUATE
               NOT INVALID KEY
                   DISPLAY 'Successfully read: ' EMP-ID ' - ' EMP-NAME
           END-READ.
           
      *    測試不存在的記錄
           MOVE 'E99999' TO EMP-ID.
           MOVE EMP-ID TO WS-TARGET-KEY.
           
           READ EMPLOYEE-FILE
               INVALID KEY
                   DISPLAY 'Expected: Record not found for ' EMP-ID
               NOT INVALID KEY
                   DISPLAY 'Unexpected: Found record ' EMP-ID
           END-READ.
           
       SAFE-WRITE-OPERATION.
           DISPLAY 'Testing safe write operations...'.
           
           MOVE 'WRITE' TO WS-OPERATION-TYPE.
           
      *    準備測試資料
           MOVE 'T00001' TO EMP-ID.
           MOVE EMP-ID TO WS-TARGET-KEY.
           MOVE 'Test Employee' TO EMP-NAME.
           MOVE 'IT01' TO EMP-DEPT.
           MOVE 'Tester' TO EMP-POSITION.
           MOVE 50000.00 TO EMP-SALARY.
           MOVE 'A' TO EMP-STATUS.
           
           WRITE EMPLOYEE-RECORD
               INVALID KEY
                   EVALUATE TRUE
                       WHEN EMP-DUPLICATE-KEY
                           DISPLAY 'Duplicate key detected: ' EMP-ID
                           PERFORM HANDLE-DUPLICATE-KEY
                       WHEN EMP-NO-SPACE
                           DISPLAY 'No space available'
                           PERFORM HANDLE-NO-SPACE
                       WHEN OTHER
                           DISPLAY 'Write error: ' WS-EMP-FILE-STATUS
                           PERFORM LOG-FILE-ERROR
                   END-EVALUATE
               NOT INVALID KEY
                   DISPLAY 'Successfully wrote: ' EMP-ID
           END-WRITE.
           
       SAFE-UPDATE-OPERATION.
           DISPLAY 'Testing safe update operations...'.
           
           MOVE 'UPDATE' TO WS-OPERATION-TYPE.
           MOVE 'T00001' TO EMP-ID.
           MOVE EMP-ID TO WS-TARGET-KEY.
           
      *    先讀取記錄
           READ EMPLOYEE-FILE
               INVALID KEY
                   DISPLAY 'Cannot read record for update: ' EMP-ID
                   PERFORM LOG-FILE-ERROR
               NOT INVALID KEY
      *            修改資料
                   MOVE 55000.00 TO EMP-SALARY
                   MOVE 'Senior Tester' TO EMP-POSITION
                   
      *            寫回更新
                   REWRITE EMPLOYEE-RECORD
                       INVALID KEY
                           EVALUATE TRUE
                               WHEN EMP-RECORD-LOCKED
                                   DISPLAY 'Record locked during update'
                                   PERFORM HANDLE-RECORD-LOCK
                               WHEN OTHER
                                   DISPLAY 'Update error: ' WS-EMP-FILE-STATUS
                                   PERFORM LOG-FILE-ERROR
                           END-EVALUATE
                       NOT INVALID KEY
                           DISPLAY 'Successfully updated: ' EMP-ID
                   END-REWRITE
           END-READ.
           
       SAFE-DELETE-OPERATION.
           DISPLAY 'Testing safe delete operations...'.
           
           MOVE 'DELETE' TO WS-OPERATION-TYPE.
           MOVE 'T00001' TO EMP-ID.
           MOVE EMP-ID TO WS-TARGET-KEY.
           
           DELETE EMPLOYEE-FILE
               INVALID KEY
                   EVALUATE TRUE
                       WHEN EMP-RECORD-NOT-FOUND
                           DISPLAY 'Record not found for delete: ' EMP-ID
                       WHEN EMP-RECORD-LOCKED
                           DISPLAY 'Record locked for delete'
                           PERFORM HANDLE-RECORD-LOCK
                       WHEN OTHER
                           DISPLAY 'Delete error: ' WS-EMP-FILE-STATUS
                           PERFORM LOG-FILE-ERROR
                   END-EVALUATE
               NOT INVALID KEY
                   DISPLAY 'Successfully deleted: ' EMP-ID
           END-DELETE.
           
       HANDLE-RECORD-LOCK.
           DISPLAY 'Handling record lock situation...'.
           
      *    等待並重試
           CALL 'QSLEEP' USING 2000.  *等待2秒
           
      *    這裡可以實作更複雜的鎖定處理邏輯
      *    例如：通知使用者、記錄等待時間、設定超時等
           
           PERFORM LOG-FILE-ERROR.
           
       HANDLE-DUPLICATE-KEY.
           DISPLAY 'Handling duplicate key situation...'.
           
      *    可以選擇更新現有記錄或提示使用者
           DISPLAY 'Option 1: Update existing record'.
           DISPLAY 'Option 2: Generate new key'.
           DISPLAY 'Option 3: Cancel operation'.
           
           PERFORM LOG-FILE-ERROR.
           
       HANDLE-NO-SPACE.
           DISPLAY 'Handling no space situation...'.
           
      *    可以清理暫存檔案、壓縮資料等
           DISPLAY 'Attempting to free up space...'.
           
           PERFORM LOG-FILE-ERROR.
           
       LOG-FILE-ERROR.
           IF LOG-SUCCESS
               MOVE WS-CURRENT-TIMESTAMP TO LOG-TIMESTAMP
               MOVE 'FILEERR' TO LOG-PROGRAM-ID
               MOVE WS-OPERATION-TYPE TO LOG-OPERATION
               MOVE 'EMPMASTER' TO LOG-FILE-NAME
               MOVE WS-TARGET-KEY TO LOG-RECORD-KEY
               MOVE WS-EMP-FILE-STATUS TO LOG-FILE-STATUS
               PERFORM GET-ERROR-MESSAGE
               MOVE WS-CURRENT-USER TO LOG-USER-ID
               MOVE WS-CURRENT-JOB TO LOG-JOB-INFO
               
               WRITE ERROR-LOG-RECORD
               
               DISPLAY 'Error logged: ' LOG-ERROR-MESSAGE
           END-IF.
           
       GET-ERROR-MESSAGE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 20
               IF WS-MSG-STATUS(WS-IDX) = WS-EMP-FILE-STATUS
                   MOVE WS-MSG-TEXT(WS-IDX) TO LOG-ERROR-MESSAGE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           IF LOG-ERROR-MESSAGE = SPACES
               STRING 'Unknown error code: ' WS-EMP-FILE-STATUS
                      DELIMITED BY SIZE
                      INTO LOG-ERROR-MESSAGE
           END-IF.
           
       SAFE-CLOSE-EMPLOYEE-FILE.
           CLOSE EMPLOYEE-FILE.
           IF NOT EMP-SUCCESS
               DISPLAY 'Warning: Error closing employee file'
               DISPLAY 'Status: ' WS-EMP-FILE-STATUS
           ELSE
               DISPLAY 'Employee file closed successfully'
           END-IF.
           
       CLEANUP-AND-EXIT.
           IF LOG-SUCCESS
               CLOSE ERROR-LOG-FILE
           END-IF.
           
           DISPLAY 'Error handling demonstration completed.'.
```

### 1.3 SQL錯誤處理

#### 1.3.1 完整的SQL錯誤處理機制

```cobol
      *****************************************************************
      * 程式名稱：SQLERR                                             *
      * 程式功能：SQL錯誤處理示範                                    *
      * 作者：資料庫專家                                             *
      * 日期：2024/02/10                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLERR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    SQL通訊區域
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
      *    SQL錯誤處理控制
       01  WS-SQL-ERROR-CONTROL.
           05  WS-SQL-OPERATION    PIC X(20).
           05  WS-SQL-RETRY-COUNT  PIC 9(2) VALUE 0.
           05  WS-MAX-SQL-RETRIES  PIC 9(2) VALUE 3.
           05  WS-SQL-ERROR-LOGGED PIC X.
               88  SQL-ERROR-LOGGED        VALUE 'Y'.
               88  SQL-ERROR-NOT-LOGGED    VALUE 'N'.
               
      *    SQL狀態分類
       01  WS-SQL-STATUS-ANALYSIS.
           05  WS-SQLSTATE-CLASS   PIC XX.
           05  WS-SQLSTATE-SUBCLASS PIC XXX.
           05  WS-ERROR-SEVERITY   PIC X(10).
               88  SEVERITY-INFO           VALUE 'INFO'.
               88  SEVERITY-WARNING        VALUE 'WARNING'.
               88  SEVERITY-ERROR          VALUE 'ERROR'.
               88  SEVERITY-FATAL          VALUE 'FATAL'.
               
      *    錯誤復原策略
       01  WS-RECOVERY-STRATEGY.
           05  WS-RECOVERY-ACTION  PIC X(20).
               88  RECOVERY-RETRY          VALUE 'RETRY'.
               88  RECOVERY-ROLLBACK       VALUE 'ROLLBACK'.
               88  RECOVERY-COMMIT         VALUE 'COMMIT'.
               88  RECOVERY-ABORT          VALUE 'ABORT'.
               88  RECOVERY-CONTINUE       VALUE 'CONTINUE'.
               
      *    測試資料
       01  WS-TEST-DATA.
           05  WS-EMP-ID           PIC X(6).
           05  WS-EMP-NAME         PIC X(30).
           05  WS-DEPT-CODE        PIC X(4).
           05  WS-SALARY           PIC 9(7)V99 COMP-3.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-SQL-ERROR-HANDLING
           PERFORM DEMONSTRATE-SQL-OPERATIONS
           PERFORM FINALIZE-SQL-OPERATIONS.
           STOP RUN.
           
       INITIALIZE-SQL-ERROR-HANDLING.
           DISPLAY 'Initializing SQL Error Handling...'.
           
      *    設定SQL預設處理
           EXEC SQL
               WHENEVER SQLERROR CONTINUE
           END-EXEC.
           
           EXEC SQL
               WHENEVER SQLWARNING CONTINUE  
           END-EXEC.
           
           EXEC SQL
               WHENEVER NOT FOUND CONTINUE
           END-EXEC.
           
           DISPLAY 'SQL Error handling initialized.'.
           
       DEMONSTRATE-SQL-OPERATIONS.
           DISPLAY 'Demonstrating SQL Operations with Error Handling...'.
           
           PERFORM TEST-SELECT-OPERATIONS
           PERFORM TEST-INSERT-OPERATIONS
           PERFORM TEST-UPDATE-OPERATIONS
           PERFORM TEST-DELETE-OPERATIONS
           PERFORM TEST-TRANSACTION-OPERATIONS.
           
       TEST-SELECT-OPERATIONS.
           DISPLAY 'Testing SELECT operations...'.
           
      *    正常查詢
           MOVE 'SELECT-NORMAL' TO WS-SQL-OPERATION.
           
           EXEC SQL
               SELECT EMP_NAME, DEPT_CODE, SALARY
               INTO :WS-EMP-NAME, :WS-DEPT-CODE, :WS-SALARY
               FROM LIBRARY/EMPLOYEES
               WHERE EMP_ID = 'E00001'
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
      *    查詢不存在的記錄
           MOVE 'SELECT-NOT-FOUND' TO WS-SQL-OPERATION.
           
           EXEC SQL
               SELECT EMP_NAME, DEPT_CODE, SALARY
               INTO :WS-EMP-NAME, :WS-DEPT-CODE, :WS-SALARY
               FROM LIBRARY/EMPLOYEES
               WHERE EMP_ID = 'XXXXX'
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
      *    語法錯誤查詢（故意的）
           MOVE 'SELECT-SYNTAX-ERROR' TO WS-SQL-OPERATION.
           
           EXEC SQL
               SELECT INVALID_COLUMN
               INTO :WS-EMP-NAME
               FROM LIBRARY/EMPLOYEES
               WHERE EMP_ID = 'E00001'
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
       TEST-INSERT-OPERATIONS.
           DISPLAY 'Testing INSERT operations...'.
           
      *    正常插入
           MOVE 'INSERT-NORMAL' TO WS-SQL-OPERATION.
           
           EXEC SQL
               INSERT INTO LIBRARY/EMPLOYEES (
                   EMP_ID, EMP_NAME, DEPT_CODE, SALARY, STATUS
               ) VALUES (
                   'T00002', '測試員工2', 'IT01', 45000.00, 'A'
               )
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
      *    重複鍵插入
           MOVE 'INSERT-DUPLICATE' TO WS-SQL-OPERATION.
           
           EXEC SQL
               INSERT INTO LIBRARY/EMPLOYEES (
                   EMP_ID, EMP_NAME, DEPT_CODE, SALARY, STATUS
               ) VALUES (
                   'T00002', '重複測試', 'HR01', 50000.00, 'A'
               )
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
      *    違反約束的插入
           MOVE 'INSERT-CONSTRAINT' TO WS-SQL-OPERATION.
           
           EXEC SQL
               INSERT INTO LIBRARY/EMPLOYEES (
                   EMP_ID, EMP_NAME, DEPT_CODE, SALARY, STATUS
               ) VALUES (
                   'T00003', '約束測試', 'XXXX', -1000.00, 'X'
               )
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
       TEST-UPDATE-OPERATIONS.
           DISPLAY 'Testing UPDATE operations...'.
           
      *    正常更新
           MOVE 'UPDATE-NORMAL' TO WS-SQL-OPERATION.
           
           EXEC SQL
               UPDATE LIBRARY/EMPLOYEES
               SET SALARY = SALARY * 1.05
               WHERE EMP_ID = 'T00002'
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
      *    更新不存在的記錄
           MOVE 'UPDATE-NOT-FOUND' TO WS-SQL-OPERATION.
           
           EXEC SQL
               UPDATE LIBRARY/EMPLOYEES
               SET SALARY = 60000.00
               WHERE EMP_ID = 'YYYYY'
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
      *    違反約束的更新
           MOVE 'UPDATE-CONSTRAINT' TO WS-SQL-OPERATION.
           
           EXEC SQL
               UPDATE LIBRARY/EMPLOYEES
               SET SALARY = -5000.00
               WHERE EMP_ID = 'T00002'
           END-EXEC.
           
           PERFORM ANALYZE-SQL-RESULT.
           
       TEST-DELETE-OPERATIONS.
           DISPLAY 'Testing DELETE operations...'.
           
      *    正常刪除
           MOVE 'DELETE-NORMAL' TO WS-SQL-OPERATION.
           
           EXEC SQL
               DELETE FROM LIBRARY/EMPLOYEES
               WHERE E