# 第五週：資料庫整合 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解DB2/400資料庫架構和特色
- 掌握SQL語言在AS/400環境中的應用
- 熟練使用Embedded SQL進行資料庫操作
- 實作複雜的資料查詢和處理邏輯
- 處理資料庫異常和錯誤狀況
- 設計高效的資料存取策略和效能優化

---

## 🗃️ 第一節：DB2/400 資料庫基礎

### 1.1 DB2/400 架構概論

#### 1.1.1 資料庫系統特色

```
DB2/400 關聯式資料庫管理系統特點：
┌─────────────────────────────────────────┐
│ 1. 完全整合的關聯式資料庫              │
│    - 與作業系統深度整合                │
│    - 單一系統映像 (Single System Image) │
│    - 統一的安全管理機制                │
│                                         │
│ 2. 多重存取介面                        │
│    - 傳統檔案存取 (Record Level)        │
│    - SQL介面 (Set Level)               │
│    - ODBC/JDBC連接                     │
│                                         │
│ 3. 高效能和可靠性                      │
│    - 自動效能調校                      │
│    - 內建備份和回復機制                │
│    - 多重索引支援                      │
│                                         │
│ 4. ANSI SQL標準相容                    │
│    - 支援標準SQL語法                   │
│    - 擴充的SQL功能                     │
│    - 程序化SQL (SQL PL)                │
└─────────────────────────────────────────┘
```

#### 1.1.2 資料庫物件階層

```sql
-- DB2/400 物件階層結構
/*
AS/400系統
├── 程式庫 (Library/Schema)
│   ├── 資料表 (Table/Physical File)
│   │   ├── 欄位 (Column/Field)
│   │   ├── 索引 (Index/Logical File)
│   │   └── 約束 (Constraint)
│   ├── 檢視 (View/Logical File)
│   ├── 預存程序 (Stored Procedure)
│   ├── 函數 (Function)
│   └── 觸發程式 (Trigger)
└── 系統程式庫
    ├── QSYS - 系統物件
    ├── QSYS2 - 系統檢視
    └── QSYSINC - 系統包含檔
*/
```

### 1.2 資料表設計和建立

#### 1.2.1 CREATE TABLE 語法

```sql
-- 員工主檔資料表
CREATE TABLE LIBRARY/EMPLOYEES (
    EMP_ID          CHAR(6)        NOT NULL PRIMARY KEY,
    EMP_NAME        VARCHAR(30)    NOT NULL,
    DEPT_CODE       CHAR(4)        NOT NULL,
    POSITION_CODE   VARCHAR(15),
    SALARY          DECIMAL(9,2)   NOT NULL DEFAULT 0,
    HIRE_DATE       DATE           NOT NULL DEFAULT CURRENT DATE,
    STATUS          CHAR(1)        NOT NULL DEFAULT 'A',
    EMAIL           VARCHAR(50),
    PHONE           VARCHAR(15),
    SUPERVISOR_ID   CHAR(6),
    CREATED_BY      VARCHAR(10)    NOT NULL DEFAULT USER,
    CREATED_TS      TIMESTAMP      NOT NULL DEFAULT CURRENT TIMESTAMP,
    UPDATED_BY      VARCHAR(10),
    UPDATED_TS      TIMESTAMP
) IN LIBRARY;

-- 為資料表增加註解
COMMENT ON TABLE LIBRARY/EMPLOYEES IS '員工主檔';
COMMENT ON COLUMN LIBRARY/EMPLOYEES.EMP_ID IS '員工編號';
COMMENT ON COLUMN LIBRARY/EMPLOYEES.EMP_NAME IS '員工姓名';
COMMENT ON COLUMN LIBRARY/EMPLOYEES.DEPT_CODE IS '部門代碼';
COMMENT ON COLUMN LIBRARY/EMPLOYEES.SALARY IS '月薪';
```

#### 1.2.2 約束定義

```sql
-- 部門主檔
CREATE TABLE LIBRARY/DEPARTMENTS (
    DEPT_CODE       CHAR(4)        NOT NULL PRIMARY KEY,
    DEPT_NAME       VARCHAR(30)    NOT NULL,
    MANAGER_ID      CHAR(6),
    BUDGET          DECIMAL(12,2)  DEFAULT 0,
    LOCATION        VARCHAR(20)
) IN LIBRARY;

-- 增加外來鍵約束
ALTER TABLE LIBRARY/EMPLOYEES 
ADD CONSTRAINT FK_EMP_DEPT 
FOREIGN KEY (DEPT_CODE) 
REFERENCES LIBRARY/DEPARTMENTS (DEPT_CODE);

ALTER TABLE LIBRARY/EMPLOYEES 
ADD CONSTRAINT FK_EMP_SUPERVISOR 
FOREIGN KEY (SUPERVISOR_ID) 
REFERENCES LIBRARY/EMPLOYEES (EMP_ID);

-- 增加檢查約束
ALTER TABLE LIBRARY/EMPLOYEES 
ADD CONSTRAINT CHK_EMP_STATUS 
CHECK (STATUS IN ('A', 'I', 'R'));

ALTER TABLE LIBRARY/EMPLOYEES 
ADD CONSTRAINT CHK_EMP_SALARY 
CHECK (SALARY >= 0 AND SALARY <= 999999.99);
```

#### 1.2.3 索引建立

```sql
-- 建立各種類型的索引
CREATE INDEX LIBRARY/IX_EMP_NAME 
ON LIBRARY/EMPLOYEES (EMP_NAME);

CREATE INDEX LIBRARY/IX_EMP_DEPT 
ON LIBRARY/EMPLOYEES (DEPT_CODE, EMP_NAME);

CREATE INDEX LIBRARY/IX_EMP_SALARY 
ON LIBRARY/EMPLOYEES (SALARY DESC);

-- 建立複合索引
CREATE INDEX LIBRARY/IX_EMP_COMPOSITE 
ON LIBRARY/EMPLOYEES (DEPT_CODE, SALARY DESC, HIRE_DATE);

-- 建立唯一索引
CREATE UNIQUE INDEX LIBRARY/IX_EMP_EMAIL 
ON LIBRARY/EMPLOYEES (EMAIL);
```

### 1.3 資料操作語言 (DML)

#### 1.3.1 基本 INSERT 操作

```sql
-- 新增單筆記錄
INSERT INTO LIBRARY/DEPARTMENTS VALUES (
    'IT01', 'Information Technology', 'E00001', 500000.00, 'Taipei'
);

INSERT INTO LIBRARY/DEPARTMENTS VALUES (
    'HR01', 'Human Resources', 'E00002', 200000.00, 'Taipei'
), (
    'FN01', 'Finance', 'E00003', 300000.00, 'Taipei'
), (
    'SL01', 'Sales', 'E00004', 800000.00, 'Kaohsiung'
);

-- 新增員工記錄
INSERT INTO LIBRARY/EMPLOYEES (
    EMP_ID, EMP_NAME, DEPT_CODE, POSITION_CODE, SALARY, HIRE_DATE, EMAIL
) VALUES (
    'E00001', '張志明', 'IT01', 'IT Manager', 80000.00, '2020-01-15', 'zhang@company.com'
), (
    'E00002', '李美玲', 'HR01', 'HR Manager', 70000.00, '2020-02-01', 'li@company.com'
), (
    'E00003', '王建國', 'FN01', 'Finance Manager', 75000.00, '2020-03-01', 'wang@company.com'
);
```

#### 1.3.2 複雜 SELECT 查詢

```sql
-- 基本查詢
SELECT EMP_ID, EMP_NAME, DEPT_CODE, SALARY 
FROM LIBRARY/EMPLOYEES 
WHERE STATUS = 'A' 
ORDER BY SALARY DESC;

-- 聯結查詢
SELECT E.EMP_ID, E.EMP_NAME, D.DEPT_NAME, E.SALARY,
       S.EMP_NAME AS SUPERVISOR_NAME
FROM LIBRARY/EMPLOYEES E
JOIN LIBRARY/DEPARTMENTS D ON E.DEPT_CODE = D.DEPT_CODE
LEFT JOIN LIBRARY/EMPLOYEES S ON E.SUPERVISOR_ID = S.EMP_ID
WHERE E.STATUS = 'A'
ORDER BY D.DEPT_NAME, E.SALARY DESC;

-- 聚合查詢
SELECT D.DEPT_NAME,
       COUNT(*) AS EMPLOYEE_COUNT,
       AVG(E.SALARY) AS AVG_SALARY,
       MIN(E.SALARY) AS MIN_SALARY,
       MAX(E.SALARY) AS MAX_SALARY,
       SUM(E.SALARY) AS TOTAL_SALARY
FROM LIBRARY/EMPLOYEES E
JOIN LIBRARY/DEPARTMENTS D ON E.DEPT_CODE = D.DEPT_CODE
WHERE E.STATUS = 'A'
GROUP BY D.DEPT_CODE, D.DEPT_NAME
HAVING COUNT(*) > 1
ORDER BY AVG_SALARY DESC;
```

#### 1.3.3 UPDATE 和 DELETE 操作

```sql
-- 更新員工薪資
UPDATE LIBRARY/EMPLOYEES 
SET SALARY = SALARY * 1.05,
    UPDATED_BY = 'SYSTEM',
    UPDATED_TS = CURRENT TIMESTAMP
WHERE DEPT_CODE = 'IT01' 
  AND STATUS = 'A'
  AND HIRE_DATE < CURRENT DATE - 1 YEAR;

-- 條件式更新
UPDATE LIBRARY/EMPLOYEES 
SET POSITION_CODE = CASE 
    WHEN SALARY >= 80000 THEN 'Senior ' || POSITION_CODE
    WHEN SALARY >= 60000 THEN 'Mid ' || POSITION_CODE
    ELSE 'Junior ' || POSITION_CODE
END,
UPDATED_TS = CURRENT TIMESTAMP
WHERE STATUS = 'A';

-- 刪除離職員工記錄 (謹慎使用)
DELETE FROM LIBRARY/EMPLOYEES 
WHERE STATUS = 'R' 
  AND UPDATED_TS < CURRENT TIMESTAMP - 2 YEARS;
```

---

## 💾 第二節：Embedded SQL 基礎

### 2.1 Embedded SQL 概念

#### 2.1.1 基本語法結構

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQL-DEMO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    SQL通訊區域
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
      *    主變數定義
       01  WS-HOST-VARIABLES.
           05  WS-EMP-ID           PIC X(6).
           05  WS-EMP-NAME         PIC X(30).
           05  WS-DEPT-CODE        PIC X(4).
           05  WS-SALARY           PIC S9(7)V99 COMP-3.
           05  WS-HIRE-DATE        PIC X(10).
           
      *    指示變數
       01  WS-INDICATOR-VARIABLES.
           05  WS-EMP-NAME-IND     PIC S9(4) COMP.
           05  WS-DEPT-CODE-IND    PIC S9(4) COMP.
           05  WS-SALARY-IND       PIC S9(4) COMP.
           05  WS-HIRE-DATE-IND    PIC S9(4) COMP.
           
      *    SQL狀態檢查
       01  WS-SQL-STATUS.
           05  WS-SQLCODE          PIC S9(8) COMP.
           05  WS-SQLSTATE         PIC X(5).
```

#### 2.1.2 SQL錯誤處理

```cobol
       PROCEDURE DIVISION.
       
       CHECK-SQL-STATUS.
           IF SQLCODE = 0
               CONTINUE
           ELSE
               IF SQLCODE = 100
                   DISPLAY 'INFO: No data found'
               ELSE
                   DISPLAY 'SQL ERROR: ' SQLCODE
                   DISPLAY 'SQLSTATE: ' SQLSTATE
                   DISPLAY 'SQLMSG: ' SQLERRMC
                   PERFORM ROLLBACK-TRANSACTION
                   STOP RUN
               END-IF
           END-IF.
           
       ROLLBACK-TRANSACTION.
           EXEC SQL
               ROLLBACK WORK
           END-EXEC.
```

### 2.2 基本 SQL 操作

#### 2.2.1 單筆記錄查詢

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
       01  WS-EMPLOYEE-INFO.
           05  WS-EMP-ID           PIC X(6).
           05  WS-EMP-NAME         PIC X(30).
           05  WS-DEPT-NAME        PIC X(30).
           05  WS-POSITION         PIC X(15).
           05  WS-SALARY           PIC S9(7)V99 COMP-3.
           05  WS-HIRE-DATE        PIC X(10).
           
       01  WS-SEARCH-KEY           PIC X(6).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'Enter Employee ID: '.
           ACCEPT WS-SEARCH-KEY.
           
           PERFORM SEARCH-EMPLOYEE.
           
           IF SQLCODE = 0
               PERFORM DISPLAY-EMPLOYEE-INFO
           ELSE
               IF SQLCODE = 100
                   DISPLAY 'Employee not found: ' WS-SEARCH-KEY
               ELSE
                   PERFORM CHECK-SQL-STATUS
               END-IF
           END-IF.
           
           STOP RUN.
           
       SEARCH-EMPLOYEE.
           EXEC SQL
               SELECT E.EMP_ID, E.EMP_NAME, D.DEPT_NAME, 
                      E.POSITION_CODE, E.SALARY, 
                      CHAR(E.HIRE_DATE, ISO)
               INTO :WS-EMP-ID, :WS-EMP-NAME, :WS-DEPT-NAME,
                    :WS-POSITION, :WS-SALARY, :WS-HIRE-DATE
               FROM LIBRARY/EMPLOYEES E
               JOIN LIBRARY/DEPARTMENTS D ON E.DEPT_CODE = D.DEPT_CODE
               WHERE E.EMP_ID = :WS-SEARCH-KEY
                 AND E.STATUS = 'A'
           END-EXEC.
           
       DISPLAY-EMPLOYEE-INFO.
           DISPLAY '========== Employee Information =========='.
           DISPLAY 'ID: ' WS-EMP-ID.
           DISPLAY 'Name: ' WS-EMP-NAME.
           DISPLAY 'Department: ' WS-DEPT-NAME.
           DISPLAY 'Position: ' WS-POSITION.
           DISPLAY 'Salary: ' WS-SALARY.
           DISPLAY 'Hire Date: ' WS-HIRE-DATE.
           DISPLAY '========================================='.
```

#### 2.2.2 新增記錄

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
       01  WS-NEW-EMPLOYEE.
           05  WS-NEW-EMP-ID       PIC X(6).
           05  WS-NEW-EMP-NAME     PIC X(30).
           05  WS-NEW-DEPT-CODE    PIC X(4).
           05  WS-NEW-POSITION     PIC X(15).
           05  WS-NEW-SALARY       PIC S9(7)V99 COMP-3.
           05  WS-NEW-EMAIL        PIC X(50).
           05  WS-NEW-PHONE        PIC X(15).
           
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM GET-NEW-EMPLOYEE-DATA
           PERFORM INSERT-NEW-EMPLOYEE
           STOP RUN.
           
       GET-NEW-EMPLOYEE-DATA.
           DISPLAY '=== New Employee Registration ==='.
           DISPLAY 'Employee ID: '.
           ACCEPT WS-NEW-EMP-ID.
           DISPLAY 'Employee Name: '.
           ACCEPT WS-NEW-EMP-NAME.
           DISPLAY 'Department Code: '.
           ACCEPT WS-NEW-DEPT-CODE.
           DISPLAY 'Position: '.
           ACCEPT WS-NEW-POSITION.
           DISPLAY 'Salary: '.
           ACCEPT WS-NEW-SALARY.
           DISPLAY 'Email: '.
           ACCEPT WS-NEW-EMAIL.
           DISPLAY 'Phone: '.
           ACCEPT WS-NEW-PHONE.
           
       INSERT-NEW-EMPLOYEE.
           EXEC SQL
               INSERT INTO LIBRARY/EMPLOYEES (
                   EMP_ID, EMP_NAME, DEPT_CODE, POSITION_CODE,
                   SALARY, EMAIL, PHONE, HIRE_DATE, STATUS
               ) VALUES (
                   :WS-NEW-EMP-ID, :WS-NEW-EMP-NAME, :WS-NEW-DEPT-CODE,
                   :WS-NEW-POSITION, :WS-NEW-SALARY, :WS-NEW-EMAIL,
                   :WS-NEW-PHONE, CURRENT DATE, 'A'
               )
           END-EXEC.
           
           IF SQLCODE = 0
               DISPLAY 'Employee successfully added: ' WS-NEW-EMP-ID
               EXEC SQL
                   COMMIT WORK
               END-EXEC
           ELSE
               DISPLAY 'Failed to add employee'
               PERFORM CHECK-SQL-STATUS
               EXEC SQL
                   ROLLBACK WORK
               END-EXEC
           END-IF.
```

#### 2.2.3 更新記錄

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
       01  WS-UPDATE-DATA.
           05  WS-TARGET-EMP-ID    PIC X(6).
           05  WS-NEW-SALARY       PIC S9(7)V99 COMP-3.
           05  WS-NEW-POSITION     PIC X(15).
           05  WS-INCREASE-PERCENT PIC S9(3)V99 COMP-3.
           
       01  WS-ROWS-UPDATED         PIC S9(8) COMP.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM SALARY-UPDATE-MENU.
           STOP RUN.
           
       SALARY-UPDATE-MENU.
           DISPLAY '=== Salary Update Options ==='.
           DISPLAY '1. Update individual employee'.
           DISPLAY '2. Department-wide increase'.
           DISPLAY '3. Performance-based increase'.
           DISPLAY 'Select option: '.
           ACCEPT WS-MENU-CHOICE.
           
           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM UPDATE-INDIVIDUAL-SALARY
               WHEN '2'
                   PERFORM UPDATE-DEPARTMENT-SALARY
               WHEN '3'
                   PERFORM UPDATE-PERFORMANCE-SALARY
               WHEN OTHER
                   DISPLAY 'Invalid option'
           END-EVALUATE.
           
       UPDATE-INDIVIDUAL-SALARY.
           DISPLAY 'Employee ID: '.
           ACCEPT WS-TARGET-EMP-ID.
           DISPLAY 'New Salary: '.
           ACCEPT WS-NEW-SALARY.
           DISPLAY 'New Position: '.
           ACCEPT WS-NEW-POSITION.
           
           EXEC SQL
               UPDATE LIBRARY/EMPLOYEES
               SET SALARY = :WS-NEW-SALARY,
                   POSITION_CODE = :WS-NEW-POSITION,
                   UPDATED_BY = USER,
                   UPDATED_TS = CURRENT TIMESTAMP
               WHERE EMP_ID = :WS-TARGET-EMP-ID
                 AND STATUS = 'A'
           END-EXEC.
           
           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY 'Employee updated successfully'
                   EXEC SQL COMMIT WORK END-EXEC
               WHEN 100
                   DISPLAY 'Employee not found'
               WHEN OTHER
                   PERFORM CHECK-SQL-STATUS
                   EXEC SQL ROLLBACK WORK END-EXEC
           END-EVALUATE.
           
       UPDATE-DEPARTMENT-SALARY.
           DISPLAY 'Department Code: '.
           ACCEPT WS-NEW-DEPT-CODE.
           DISPLAY 'Increase Percentage: '.
           ACCEPT WS-INCREASE-PERCENT.
           
           EXEC SQL
               UPDATE LIBRARY/EMPLOYEES
               SET SALARY = SALARY * (1 + :WS-INCREASE-PERCENT / 100),
                   UPDATED_BY = USER,
                   UPDATED_TS = CURRENT TIMESTAMP
               WHERE DEPT_CODE = :WS-NEW-DEPT-CODE
                 AND STATUS = 'A'
           END-EXEC.
           
           IF SQLCODE = 0
               MOVE SQLERRD(3) TO WS-ROWS-UPDATED
               DISPLAY 'Updated ' WS-ROWS-UPDATED ' employees'
               EXEC SQL COMMIT WORK END-EXEC
           ELSE
               PERFORM CHECK-SQL-STATUS
               EXEC SQL ROLLBACK WORK END-EXEC
           END-IF.
```

---

## 🔄 第三節：游標操作

### 3.1 游標基本概念

#### 3.1.1 游標定義和使用

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
      *    游標處理用的主變數
       01  WS-CURSOR-DATA.
           05  WS-EMP-ID           PIC X(6).
           05  WS-EMP-NAME         PIC X(30).
           05  WS-DEPT-NAME        PIC X(30).
           05  WS-SALARY           PIC S9(7)V99 COMP-3.
           05  WS-HIRE-DATE        PIC X(10).
           
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(6) VALUE 0.
           05  WS-TOTAL-SALARY     PIC S9(10)V99 COMP-3 VALUE 0.
           
      *    游標狀態
       01  WS-CURSOR-STATUS        PIC X.
           88  CURSOR-FOUND            VALUE 'Y'.
           88  CURSOR-NOT-FOUND        VALUE 'N'.
           
       PROCEDURE DIVISION.
       
      *    定義游標
           EXEC SQL
               DECLARE EMP_CURSOR CURSOR FOR
               SELECT E.EMP_ID, E.EMP_NAME, D.DEPT_NAME, 
                      E.SALARY, CHAR(E.HIRE_DATE, ISO)
               FROM LIBRARY/EMPLOYEES E
               JOIN LIBRARY/DEPARTMENTS D ON E.DEPT_CODE = D.DEPT_CODE
               WHERE E.STATUS = 'A'
               ORDER BY D.DEPT_NAME, E.EMP_NAME
           END-EXEC.
           
       MAIN-LOGIC.
           PERFORM OPEN-CURSOR
           PERFORM PROCESS-ALL-EMPLOYEES
           PERFORM CLOSE-CURSOR
           PERFORM DISPLAY-SUMMARY
           STOP RUN.
           
       OPEN-CURSOR.
           EXEC SQL
               OPEN EMP_CURSOR
           END-EXEC.
           
           IF SQLCODE NOT = 0
               DISPLAY 'Error opening cursor'
               PERFORM CHECK-SQL-STATUS
               STOP RUN
           END-IF.
           
       PROCESS-ALL-EMPLOYEES.
           PERFORM FETCH-EMPLOYEE
           PERFORM UNTIL CURSOR-NOT-FOUND
               PERFORM PROCESS-EMPLOYEE-RECORD
               PERFORM FETCH-EMPLOYEE
           END-PERFORM.
           
       FETCH-EMPLOYEE.
           EXEC SQL
               FETCH EMP_CURSOR
               INTO :WS-EMP-ID, :WS-EMP-NAME, :WS-DEPT-NAME,
                    :WS-SALARY, :WS-HIRE-DATE
           END-EXEC.
           
           IF SQLCODE = 0
               SET CURSOR-FOUND TO TRUE
           ELSE
               IF SQLCODE = 100
                   SET CURSOR-NOT-FOUND TO TRUE
               ELSE
                   PERFORM CHECK-SQL-STATUS
                   STOP RUN
               END-IF
           END-IF.
           
       PROCESS-EMPLOYEE-RECORD.
           ADD 1 TO WS-RECORD-COUNT.
           ADD WS-SALARY TO WS-TOTAL-SALARY.
           
           DISPLAY WS-EMP-ID ' | ' WS-EMP-NAME ' | ' 
                   WS-DEPT-NAME ' | ' WS-SALARY.
           
       CLOSE-CURSOR.
           EXEC SQL
               CLOSE EMP_CURSOR
           END-EXEC.
           
       DISPLAY-SUMMARY.
           DISPLAY ' '.
           DISPLAY '========== Summary =========='.
           DISPLAY 'Total Employees: ' WS-RECORD-COUNT.
           DISPLAY 'Total Salary: ' WS-TOTAL-SALARY.
           IF WS-RECORD-COUNT > 0
               COMPUTE WS-AVG-SALARY = WS-TOTAL-SALARY / WS-RECORD-COUNT
               DISPLAY 'Average Salary: ' WS-AVG-SALARY
           END-IF.
```

### 3.2 可更新游標

#### 3.2.1 FOR UPDATE 游標

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
       01  WS-UPDATE-DATA.
           05  WS-EMP-ID           PIC X(6).
           05  WS-CURRENT-SALARY   PIC S9(7)V99 COMP-3.
           05  WS-NEW-SALARY       PIC S9(7)V99 COMP-3.
           05  WS-DEPT-CODE        PIC X(4).
           05  WS-HIRE-DATE        PIC X(10).
           05  WS-SERVICE-YEARS    PIC S9(3) COMP-3.
           
       01  WS-ADJUSTMENT-RATES.
           05  WS-RATE-0-2-YEARS   PIC S9V999 COMP-3 VALUE 0.03.
           05  WS-RATE-3-5-YEARS   PIC S9V999 COMP-3 VALUE 0.05.
           05  WS-RATE-6-10-YEARS  PIC S9V999 COMP-3 VALUE 0.07.
           05  WS-RATE-OVER-10     PIC S9V999 COMP-3 VALUE 0.10.
           
       01  WS-COUNTERS.
           05  WS-UPDATED-COUNT    PIC 9(6) VALUE 0.
           
       PROCEDURE DIVISION.
       
      *    定義可更新游標
           EXEC SQL
               DECLARE SALARY_UPDATE_CURSOR CURSOR FOR
               SELECT EMP_ID, SALARY, DEPT_CODE, 
                      CHAR(HIRE_DATE, ISO),
                      DAYS(CURRENT DATE) - DAYS(HIRE_DATE)
               FROM LIBRARY/EMPLOYEES
               WHERE STATUS = 'A'
                 AND SALARY > 0
               FOR UPDATE OF SALARY
           END-EXEC.
           
       MAIN-LOGIC.
           DISPLAY 'Starting annual salary adjustment...'.
           
           PERFORM OPEN-UPDATE-CURSOR
           PERFORM PROCESS-SALARY-ADJUSTMENTS
           PERFORM CLOSE-UPDATE-CURSOR
           
           DISPLAY 'Salary adjustment completed.'.
           DISPLAY 'Total employees updated: ' WS-UPDATED-COUNT.
           
           STOP RUN.
           
       OPEN-UPDATE-CURSOR.
           EXEC SQL
               OPEN SALARY_UPDATE_CURSOR
           END-EXEC.
           
           IF SQLCODE NOT = 0
               DISPLAY 'Error opening update cursor'
               PERFORM CHECK-SQL-STATUS
               STOP RUN
           END-IF.
           
       PROCESS-SALARY-ADJUSTMENTS.
           PERFORM FETCH-FOR-UPDATE
           PERFORM UNTIL SQLCODE = 100
               PERFORM CALCULATE-NEW-SALARY
               PERFORM UPDATE-CURRENT-RECORD
               PERFORM FETCH-FOR-UPDATE
           END-PERFORM.
           
       FETCH-FOR-UPDATE.
           EXEC SQL
               FETCH SALARY_UPDATE_CURSOR
               INTO :WS-EMP-ID, :WS-CURRENT-SALARY, :WS-DEPT-CODE,
                    :WS-HIRE-DATE, :WS-SERVICE-YEARS
           END-EXEC.
           
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               PERFORM CHECK-SQL-STATUS
               STOP RUN
           END-IF.
           
       CALCULATE-NEW-SALARY.
           COMPUTE WS-SERVICE-YEARS = WS-SERVICE-YEARS / 365.
           
           EVALUATE TRUE
               WHEN WS-SERVICE-YEARS < 3
                   COMPUTE WS-NEW-SALARY = 
                       WS-CURRENT-SALARY * (1 + WS-RATE-0-2-YEARS)
               WHEN WS-SERVICE-YEARS < 6
                   COMPUTE WS-NEW-SALARY = 
                       WS-CURRENT-SALARY * (1 + WS-RATE-3-5-YEARS)
               WHEN WS-SERVICE-YEARS < 11
                   COMPUTE WS-NEW-SALARY = 
                       WS-CURRENT-SALARY * (1 + WS-RATE-6-10-YEARS)
               WHEN OTHER
                   COMPUTE WS-NEW-SALARY = 
                       WS-CURRENT-SALARY * (1 + WS-RATE-OVER-10)
           END-EVALUATE.
           
      *    額外部門調整
           IF WS-DEPT-CODE = 'IT01'
               COMPUTE WS-NEW-SALARY = WS-NEW-SALARY * 1.02
           END-IF.
           
       UPDATE-CURRENT-RECORD.
           EXEC SQL
               UPDATE LIBRARY/