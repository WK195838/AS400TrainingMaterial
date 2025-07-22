# 第三週：流程控制與邏輯 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 掌握各種條件判斷語法和邏輯運算
- 熟練使用迴圈控制結構處理重複性工作
- 理解程式模組化的概念和實作方法
- 設計複雜的程式邏輯流程
- 編寫結構化、可維護的COBOL程式

---

## 🔀 第一節：條件判斷基礎

### 1.1 IF-THEN-ELSE 結構

IF敘述是程式邏輯控制的核心，讓程式能夠根據不同條件執行不同的動作。

#### 1.1.1 基本語法結構

```cobol
       IF 條件
           敘述1
       ELSE
           敘述2
       END-IF.
```

#### 1.1.2 簡單條件判斷

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SCORE            PIC 999.
       01  WS-RESULT           PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '請輸入分數: '.
           ACCEPT WS-SCORE.
           
      *    簡單的及格判斷
           IF WS-SCORE >= 60
               MOVE '及格' TO WS-RESULT
           ELSE
               MOVE '不及格' TO WS-RESULT
           END-IF.
           
           DISPLAY '結果: ' WS-RESULT.
```

#### 1.1.3 複合條件判斷

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AGE              PIC 999.
       01  WS-SALARY           PIC 9(6)V99.
       01  WS-DEPARTMENT       PIC X(10).
       01  WS-BONUS-RATE       PIC 9V99.
       
       PROCEDURE DIVISION.
       BONUS-CALCULATION.
           DISPLAY '請輸入年齡: '.
           ACCEPT WS-AGE.
           DISPLAY '請輸入薪資: '.
           ACCEPT WS-SALARY.
           DISPLAY '請輸入部門: '.
           ACCEPT WS-DEPARTMENT.
           
      *    複合條件判斷獎金比例
           IF WS-AGE >= 40 AND WS-SALARY > 50000
               MOVE 0.15 TO WS-BONUS-RATE
           ELSE
               IF WS-DEPARTMENT = 'SALES' OR WS-DEPARTMENT = 'MARKETING'
                   MOVE 0.12 TO WS-BONUS-RATE
               ELSE
                   MOVE 0.10 TO WS-BONUS-RATE
               END-IF
           END-IF.
           
           DISPLAY '獎金比例: ' WS-BONUS-RATE.
```

### 1.2 邏輯運算子

#### 1.2.1 比較運算子

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM1             PIC 9(5).
       01  WS-NUM2             PIC 9(5).
       01  WS-NAME1            PIC X(20).
       01  WS-NAME2            PIC X(20).
       
       PROCEDURE DIVISION.
       COMPARISON-EXAMPLES.
      *    數值比較
           IF WS-NUM1 = WS-NUM2        *等於
               DISPLAY '兩數相等'
           END-IF.
           
           IF WS-NUM1 > WS-NUM2        *大於
               DISPLAY 'NUM1 大於 NUM2'
           END-IF.
           
           IF WS-NUM1 < WS-NUM2        *小於
               DISPLAY 'NUM1 小於 NUM2'
           END-IF.
           
           IF WS-NUM1 >= WS-NUM2       *大於等於
               DISPLAY 'NUM1 大於等於 NUM2'
           END-IF.
           
           IF WS-NUM1 <= WS-NUM2       *小於等於
               DISPLAY 'NUM1 小於等於 NUM2'
           END-IF.
           
           IF WS-NUM1 NOT = WS-NUM2    *不等於
               DISPLAY '兩數不相等'
           END-IF.
           
      *    文字比較
           IF WS-NAME1 = WS-NAME2
               DISPLAY '姓名相同'
           END-IF.
           
           IF WS-NAME1 > WS-NAME2      *字典順序比較
               DISPLAY 'NAME1 在字典順序上較後'
           END-IF.
```

#### 1.2.2 邏輯運算子

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AGE              PIC 999.
       01  WS-EXPERIENCE       PIC 99.
       01  WS-EDUCATION        PIC X(10).
       01  WS-QUALIFIED        PIC X.
       
       PROCEDURE DIVISION.
       LOGIC-OPERATIONS.
           DISPLAY '請輸入年齡: '.
           ACCEPT WS-AGE.
           DISPLAY '請輸入工作經驗(年): '.
           ACCEPT WS-EXPERIENCE.
           DISPLAY '請輸入學歷: '.
           ACCEPT WS-EDUCATION.
           
      *    AND 運算 - 所有條件都必須成立
           IF WS-AGE >= 25 AND WS-EXPERIENCE >= 3 AND WS-EDUCATION = 'BACHELOR'
               DISPLAY '符合高級職位要求'
           END-IF.
           
      *    OR 運算 - 任一條件成立即可
           IF WS-AGE >= 30 OR WS-EXPERIENCE >= 5 OR WS-EDUCATION = 'MASTER'
               DISPLAY '符合主管職位要求'
           END-IF.
           
      *    NOT 運算 - 條件不成立
           IF NOT (WS-AGE < 18)
               DISPLAY '符合工作年齡要求'
           END-IF.
           
      *    複合邏輯運算
           IF (WS-AGE >= 25 AND WS-EXPERIENCE >= 2) OR 
              (WS-EDUCATION = 'MASTER' AND WS-EXPERIENCE >= 1)
               MOVE 'Y' TO WS-QUALIFIED
           ELSE
               MOVE 'N' TO WS-QUALIFIED
           END-IF.
```

### 1.3 巢狀條件判斷

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-EMPLOYEE-TYPE    PIC X(10).
       01  WS-YEARS-SERVICE    PIC 99.
       01  WS-PERFORMANCE      PIC X.
       01  WS-BONUS-AMOUNT     PIC 9(6)V99.
       
       PROCEDURE DIVISION.
       NESTED-CONDITIONS.
           DISPLAY '請輸入員工類型 (MANAGER/STAFF): '.
           ACCEPT WS-EMPLOYEE-TYPE.
           DISPLAY '請輸入服務年資: '.
           ACCEPT WS-YEARS-SERVICE.
           DISPLAY '請輸入績效等級 (A/B/C): '.
           ACCEPT WS-PERFORMANCE.
           
      *    巢狀條件判斷
           IF WS-EMPLOYEE-TYPE = 'MANAGER'
               IF WS-YEARS-SERVICE >= 5
                   IF WS-PERFORMANCE = 'A'
                       MOVE 100000 TO WS-BONUS-AMOUNT
                   ELSE
                       IF WS-PERFORMANCE = 'B'
                           MOVE 80000 TO WS-BONUS-AMOUNT
                       ELSE
                           MOVE 60000 TO WS-BONUS-AMOUNT
                       END-IF
                   END-IF
               ELSE
                   IF WS-PERFORMANCE = 'A'
                       MOVE 60000 TO WS-BONUS-AMOUNT
                   ELSE
                       IF WS-PERFORMANCE = 'B'
                           MOVE 40000 TO WS-BONUS-AMOUNT
                       ELSE
                           MOVE 20000 TO WS-BONUS-AMOUNT
                       END-IF
                   END-IF
               END-IF
           ELSE
               IF WS-YEARS-SERVICE >= 3
                   IF WS-PERFORMANCE = 'A'
                       MOVE 30000 TO WS-BONUS-AMOUNT
                   ELSE
                       IF WS-PERFORMANCE = 'B'
                           MOVE 20000 TO WS-BONUS-AMOUNT
                       ELSE
                           MOVE 10000 TO WS-BONUS-AMOUNT
                       END-IF
                   END-IF
               ELSE
                   IF WS-PERFORMANCE = 'A'
                       MOVE 15000 TO WS-BONUS-AMOUNT
                   ELSE
                       IF WS-PERFORMANCE = 'B'
                           MOVE 10000 TO WS-BONUS-AMOUNT
                       ELSE
                           MOVE 5000 TO WS-BONUS-AMOUNT
                       END-IF
                   END-IF
               END-IF
           END-IF.
           
           DISPLAY '獎金金額: ' WS-BONUS-AMOUNT.
```

---

## 🔄 第二節：EVALUATE 敘述

### 2.1 EVALUATE 基本語法

EVALUATE敘述提供了比多層IF-ELSE更清晰的多重選擇結構：

```cobol
       EVALUATE 選擇變數
           WHEN 條件1
               敘述1
           WHEN 條件2
               敘述2
           WHEN OTHER
               預設敘述
       END-EVALUATE.
```

### 2.2 簡單的EVALUATE應用

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-GRADE            PIC X.
       01  WS-DESCRIPTION      PIC X(20).
       01  WS-GPA              PIC 9V99.
       
       PROCEDURE DIVISION.
       GRADE-EVALUATION.
           DISPLAY '請輸入等級 (A/B/C/D/F): '.
           ACCEPT WS-GRADE.
           
      *    使用EVALUATE進行等級判斷
           EVALUATE WS-GRADE
               WHEN 'A'
                   MOVE '優秀' TO WS-DESCRIPTION
                   MOVE 4.00 TO WS-GPA
               WHEN 'B'
                   MOVE '良好' TO WS-DESCRIPTION
                   MOVE 3.00 TO WS-GPA
               WHEN 'C'
                   MOVE '普通' TO WS-DESCRIPTION
                   MOVE 2.00 TO WS-GPA
               WHEN 'D'
                   MOVE '及格' TO WS-DESCRIPTION
                   MOVE 1.00 TO WS-GPA
               WHEN 'F'
                   MOVE '不及格' TO WS-DESCRIPTION
                   MOVE 0.00 TO WS-GPA
               WHEN OTHER
                   MOVE '無效等級' TO WS-DESCRIPTION
                   MOVE 0.00 TO WS-GPA
           END-EVALUATE.
           
           DISPLAY '等級說明: ' WS-DESCRIPTION.
           DISPLAY 'GPA: ' WS-GPA.
```

### 2.3 數值範圍判斷

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SCORE            PIC 999.
       01  WS-LETTER-GRADE     PIC X.
       01  WS-COMMENT          PIC X(30).
       
       PROCEDURE DIVISION.
       SCORE-TO-GRADE.
           DISPLAY '請輸入分數 (0-100): '.
           ACCEPT WS-SCORE.
           
      *    使用EVALUATE進行分數範圍判斷
           EVALUATE WS-SCORE
               WHEN 90 THRU 100
                   MOVE 'A' TO WS-LETTER-GRADE
                   MOVE '優秀表現，繼續保持！' TO WS-COMMENT
               WHEN 80 THRU 89
                   MOVE 'B' TO WS-LETTER-GRADE
                   MOVE '良好表現，再接再厲！' TO WS-COMMENT
               WHEN 70 THRU 79
                   MOVE 'C' TO WS-LETTER-GRADE
                   MOVE '普通表現，需要改進！' TO WS-COMMENT
               WHEN 60 THRU 69
                   MOVE 'D' TO WS-LETTER-GRADE
                   MOVE '勉強及格，要加油！' TO WS-COMMENT
               WHEN 0 THRU 59
                   MOVE 'F' TO WS-LETTER-GRADE
                   MOVE '不及格，需要重修！' TO WS-COMMENT
               WHEN OTHER
                   MOVE '?' TO WS-LETTER-GRADE
                   MOVE '分數無效！' TO WS-COMMENT
           END-EVALUATE.
           
           DISPLAY '等級: ' WS-LETTER-GRADE.
           DISPLAY '評語: ' WS-COMMENT.
```

### 2.4 複合條件EVALUATE

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DEPARTMENT       PIC X(10).
       01  WS-LEVEL            PIC X(10).
       01  WS-SALARY-RANGE     PIC X(20).
       
       PROCEDURE DIVISION.
       SALARY-EVALUATION.
           DISPLAY '請輸入部門: '.
           ACCEPT WS-DEPARTMENT.
           DISPLAY '請輸入職級: '.
           ACCEPT WS-LEVEL.
           
      *    複合條件的EVALUATE
           EVALUATE WS-DEPARTMENT ALSO WS-LEVEL
               WHEN 'IT' ALSO 'SENIOR'
                   MOVE '80,000 - 120,000' TO WS-SALARY-RANGE
               WHEN 'IT' ALSO 'JUNIOR'
                   MOVE '45,000 - 60,000' TO WS-SALARY-RANGE
               WHEN 'SALES' ALSO 'SENIOR'
                   MOVE '60,000 - 100,000' TO WS-SALARY-RANGE
               WHEN 'SALES' ALSO 'JUNIOR'
                   MOVE '35,000 - 50,000' TO WS-SALARY-RANGE
               WHEN 'HR' ALSO 'SENIOR'
                   MOVE '65,000 - 85,000' TO WS-SALARY-RANGE
               WHEN 'HR' ALSO 'JUNIOR'
                   MOVE '40,000 - 55,000' TO WS-SALARY-RANGE
               WHEN OTHER
                   MOVE '請洽人事部門' TO WS-SALARY-RANGE
           END-EVALUATE.
           
           DISPLAY '薪資範圍: ' WS-SALARY-RANGE.
```

---

## 🔁 第三節：PERFORM 敘述與迴圈控制

### 3.1 PERFORM 基本概念

PERFORM敘述是COBOL中實現程式模組化和迴圈控制的重要工具：

```cobol
       PERFORM 段落名稱
       PERFORM 段落名稱 THRU 結束段落名稱
       PERFORM 段落名稱 次數 TIMES
       PERFORM 段落名稱 UNTIL 條件
       PERFORM 段落名稱 VARYING 變數 FROM 起始值 BY 步進值 UNTIL 條件
```

### 3.2 簡單的PERFORM應用

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER          PIC 999.
       01  WS-TOTAL            PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-VALUES
           PERFORM PROCESS-DATA
           PERFORM DISPLAY-RESULTS
           STOP RUN.
           
       INITIALIZE-VALUES.
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-TOTAL.
           DISPLAY '初始化完成'.
           
       PROCESS-DATA.
           DISPLAY '開始處理資料'.
           MOVE 100 TO WS-TOTAL.
           DISPLAY '資料處理完成'.
           
       DISPLAY-RESULTS.
           DISPLAY '計數器: ' WS-COUNTER.
           DISPLAY '總計: ' WS-TOTAL.
```

### 3.3 PERFORM TIMES - 固定次數迴圈

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER          PIC 999 VALUE 0.
       01  WS-SUM              PIC 9(6) VALUE 0.
       01  WS-LOOP-COUNT       PIC 99 VALUE 10.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '開始計算1到10的總和'.
           PERFORM CALCULATE-SUM WS-LOOP-COUNT TIMES.
           DISPLAY '計算完成，總和: ' WS-SUM.
           STOP RUN.
           
       CALCULATE-SUM.
           ADD 1 TO WS-COUNTER.
           ADD WS-COUNTER TO WS-SUM.
           DISPLAY '第' WS-COUNTER '次迴圈，目前總和: ' WS-SUM.
```

### 3.4 PERFORM UNTIL - 條件迴圈

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUMBER           PIC 9(5).
       01  WS-FACTORIAL        PIC 9(10) VALUE 1.
       01  WS-INPUT-NUM        PIC 99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '請輸入要計算階乘的數字: '.
           ACCEPT WS-INPUT-NUM.
           
           MOVE 1 TO WS-NUMBER.
           PERFORM CALCULATE-FACTORIAL 
               UNTIL WS-NUMBER > WS-INPUT-NUM.
           
           DISPLAY WS-INPUT-NUM '的階乘是: ' WS-FACTORIAL.
           STOP RUN.
           
       CALCULATE-FACTORIAL.
           MULTIPLY WS-NUMBER BY WS-FACTORIAL.
           DISPLAY WS-NUMBER '! = ' WS-FACTORIAL.
           ADD 1 TO WS-NUMBER.
```

### 3.5 PERFORM VARYING - 變數控制迴圈

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MULTIPLICATION-TABLE.
           05  WS-MULTIPLICAND  PIC 99.
           05  WS-MULTIPLIER    PIC 99.
           05  WS-RESULT        PIC 9(4).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '九九乘法表:'.
           DISPLAY ' '.
           
           PERFORM PRINT-MULTIPLICATION-TABLE
               VARYING WS-MULTIPLICAND FROM 1 BY 1 UNTIL WS-MULTIPLICAND > 9.
           
           STOP RUN.
           
       PRINT-MULTIPLICATION-TABLE.
           PERFORM PRINT-ROW
               VARYING WS-MULTIPLIER FROM 1 BY 1 UNTIL WS-MULTIPLIER > 9.
           DISPLAY ' '.
           
       PRINT-ROW.
           COMPUTE WS-RESULT = WS-MULTIPLICAND * WS-MULTIPLIER.
           DISPLAY WS-MULTIPLICAND ' x ' WS-MULTIPLIER ' = ' WS-RESULT.
```

### 3.6 巢狀PERFORM

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OUTER-COUNTER    PIC 99.
       01  WS-INNER-COUNTER    PIC 99.
       01  WS-MATRIX.
           05  WS-ROW OCCURS 5 TIMES.
               10  WS-COL OCCURS 5 TIMES PIC 999.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '建立5x5矩陣:'.
           PERFORM CREATE-MATRIX
               VARYING WS-OUTER-COUNTER FROM 1 BY 1 UNTIL WS-OUTER-COUNTER > 5.
           
           DISPLAY ' '.
           DISPLAY '顯示矩陣:'.
           PERFORM DISPLAY-MATRIX
               VARYING WS-OUTER-COUNTER FROM 1 BY 1 UNTIL WS-OUTER-COUNTER > 5.
           
           STOP RUN.
           
       CREATE-MATRIX.
           PERFORM FILL-ROW
               VARYING WS-INNER-COUNTER FROM 1 BY 1 UNTIL WS-INNER-COUNTER > 5.
           
       FILL-ROW.
           COMPUTE WS-COL(WS-OUTER-COUNTER, WS-INNER-COUNTER) = 
               WS-OUTER-COUNTER * WS-INNER-COUNTER.
           
       DISPLAY-MATRIX.
           PERFORM DISPLAY-ROW
               VARYING WS-INNER-COUNTER FROM 1 BY 1 UNTIL WS-INNER-COUNTER > 5.
           DISPLAY ' '.
           
       DISPLAY-ROW.
           DISPLAY WS-COL(WS-OUTER-COUNTER, WS-INNER-COUNTER) ' '
               WITH NO ADVANCING.
```

---

## 🧩 第四節：程式模組化

### 4.1 SECTION 與 PARAGRAPH

#### 4.1.1 基本概念

```cobol
       PROCEDURE DIVISION.
       
       MAIN-SECTION SECTION.
       MAIN-LOGIC.
           PERFORM INITIALIZATION-SECTION
           PERFORM PROCESSING-SECTION
           PERFORM TERMINATION-SECTION
           STOP RUN.
           
       INITIALIZATION-SECTION SECTION.
       INIT-VARIABLES.
           MOVE 0 TO WS-COUNTER.
           MOVE SPACES TO WS-MESSAGE.
           DISPLAY '初始化完成'.
           
       INIT-FILES.
           DISPLAY '檔案初始化'.
           
       PROCESSING-SECTION SECTION.
       PROCESS-DATA.
           DISPLAY '處理資料'.
           
       VALIDATE-DATA.
           DISPLAY '驗證資料'.
           
       TERMINATION-SECTION SECTION.
       CLEANUP.
           DISPLAY '清理資源'.
           
       FINAL-REPORT.
           DISPLAY '產生最終報告'.
```

#### 4.1.2 使用THRU執行段落範圍

```cobol
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM SETUP-PARA THRU SETUP-END.
           PERFORM PROCESS-PARA THRU PROCESS-END.
           STOP RUN.
           
       SETUP-PARA.
           DISPLAY '開始設定'.
           
       SETUP-VARIABLES.
           MOVE 0 TO WS-COUNTER.
           
       SETUP-FILES.
           DISPLAY '設定檔案'.
           
       SETUP-END.
           DISPLAY '設定完成'.
           
       PROCESS-PARA.
           DISPLAY '開始處理'.
           
       PROCESS-DATA.
           DISPLAY '處理資料'.
           
       PROCESS-END.
           DISPLAY '處理完成'.
```

### 4.2 子程式設計

#### 4.2.1 內部子程式

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-EMPLOYEE-DATA.
           05  WS-EMP-ID        PIC X(6).
           05  WS-EMP-NAME      PIC X(20).
           05  WS-EMP-SALARY    PIC 9(7)V99.
           05  WS-EMP-DEPARTMENT PIC X(10).
       
       01  WS-VALIDATION-RESULT PIC X.
           88  VALID-DATA              VALUE 'Y'.
           88  INVALID-DATA            VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM GET-EMPLOYEE-DATA
           PERFORM VALIDATE-EMPLOYEE-DATA
           
           IF VALID-DATA
               PERFORM SAVE-EMPLOYEE-DATA
               DISPLAY '員工資料儲存成功'
           ELSE
               DISPLAY '員工資料驗證失敗'
           END-IF.
           
           STOP RUN.
           
       GET-EMPLOYEE-DATA.
           DISPLAY '=== 員工資料輸入 ==='.
           DISPLAY '員工編號: '.
           ACCEPT WS-EMP-ID.
           DISPLAY '員工姓名: '.
           ACCEPT WS-EMP-NAME.
           DISPLAY '員工薪資: '.
           ACCEPT WS-EMP-SALARY.
           DISPLAY '部門: '.
           ACCEPT WS-EMP-DEPARTMENT.
           
       VALIDATE-EMPLOYEE-DATA.
           SET VALID-DATA TO TRUE.
           
      *    驗證員工編號
           IF WS-EMP-ID = SPACES
               DISPLAY '錯誤: 員工編號不能為空'
               SET INVALID-DATA TO TRUE
           END-IF.
           
      *    驗證員工姓名
           IF WS-EMP-NAME = SPACES
               DISPLAY '錯誤: 員工姓名不能為空'
               SET INVALID-DATA TO TRUE
           END-IF.
           
      *    驗證薪資
           IF WS-EMP-SALARY <= 0
               DISPLAY '錯誤: 薪資必須大於0'
               SET INVALID-DATA TO TRUE
           END-IF.
           
      *    驗證部門
           IF WS-EMP-DEPARTMENT NOT = 'IT' AND
              WS-EMP-DEPARTMENT NOT = 'SALES' AND
              WS-EMP-DEPARTMENT NOT = 'HR' AND
              WS-EMP-DEPARTMENT NOT = 'FINANCE'
               DISPLAY '錯誤: 部門代碼無效'
               SET INVALID-DATA TO TRUE
           END-IF.
           
       SAVE-EMPLOYEE-DATA.
           DISPLAY '儲存員工資料:'.
           DISPLAY '編號: ' WS-EMP-ID.
           DISPLAY '姓名: ' WS-EMP-NAME.
           DISPLAY '薪資: ' WS-EMP-SALARY.
           DISPLAY '部門: ' WS-EMP-DEPARTMENT.
```

#### 4.2.2 參數化子程式

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CALC-PARAMS.
           05  WS-PRINCIPAL     PIC 9(8)V99.
           05  WS-RATE          PIC 9V9999.
           05  WS-YEARS         PIC 99.
           05  WS-COMPOUND-INTEREST PIC 9(10)V99.
           05  WS-TOTAL-AMOUNT  PIC 9(10)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '複利計算程式'.
           PERFORM GET-CALCULATION-PARAMS
           PERFORM CALCULATE-COMPOUND-INTEREST
           PERFORM DISPLAY-RESULTS
           STOP RUN.
           
       GET-CALCULATION-PARAMS.
           DISPLAY '請輸入本金: '.
           ACCEPT WS-PRINCIPAL.
           DISPLAY '請輸入年利率 (小數): '.
           ACCEPT WS-RATE.
           DISPLAY '請輸入年數: '.
           ACCEPT WS-YEARS.
           
       CALCULATE-COMPOUND-INTEREST.
      *    使用複利公式: A = P(1 + r)^n
           COMPUTE WS-TOTAL-AMOUNT = 
               WS-PRINCIPAL * ((1 + WS-RATE) ** WS-YEARS).
           
           COMPUTE WS-COMPOUND-INTEREST = 
               WS-TOTAL-AMOUNT - WS-PRINCIPAL.
           
       DISPLAY-RESULTS.
           DISPLAY ' '.
           DISPLAY '=== 複利計算結果 ==='.
           DISPLAY '本金: ' WS-PRINCIPAL.
           DISPLAY '年利率: ' WS-RATE.
           DISPLAY '年數: ' WS-YEARS.
           DISPLAY '複利: ' WS-COMPOUND-INTEREST.
           DISPLAY '總金額: ' WS-TOTAL-AMOUNT.
```

---

## 💡 第五節：實例練習

### 實例5.1：學生成績管理系統

**需求分析：**
- 輸入多位學生的多科成績
- 計算個人平均分和總班平均
- 統計各等級人數
- 找出最高分和最低分

```cobol
      *****************************************************************
      * 程式名稱：STUDENT-GRADE-MGMT                                 *
      * 程式功能：學生成績管理系統                                   *
      * 作者：陳六                                                   *
      * 日期：2024/01/20                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-GRADE-MGMT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    常數定義
       01  WS-CONSTANTS.
           05  WS-MAX-STUDENTS     PIC 99 VALUE 30.
           05  WS-SUBJECT-COUNT    PIC 9 VALUE 5.
       
      *    學生成績資料
       01  WS-STUDENT-TABLE.
           05  WS-STUDENT OCCURS 30 TIMES.
               10  WS-NAME         PIC X(10).
               10  WS-SCORES       PIC 999 OCCURS 5 TIMES.
               10  WS-AVG          PIC 999V99.
       
      *    統計用變數
       01  WS-TOTAL-SUM           PIC 9(6)V99 VALUE 0.
       01  WS-CLASS-AVG           PIC 999V99 VALUE 0.
       01  WS-GRADE-COUNT.
           05  WS-A-COUNT         PIC 99 VALUE 0.
           05  WS-B-COUNT         PIC 99 VALUE 0.
           05  WS-C-COUNT         PIC 99 VALUE 0.
           05  WS-D-COUNT         PIC 99 VALUE 0.
           05  WS-F-COUNT         PIC 99 VALUE 0.
       01  WS-HIGHEST             PIC 999 VALUE 0.
       01  WS-LOWEST              PIC 999 VALUE 100.
       01  WS-TEMP-SCORE          PIC 999.
       01  WS-I                   PIC 99 VALUE 1.
       01  WS-J                   PIC 9 VALUE 1.
       01  WS-STUDENT-COUNT       PIC 99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '請輸入學生人數(最多30): '.
           ACCEPT WS-STUDENT-COUNT.
           
           PERFORM INPUT-STUDENT-DATA VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-STUDENT-COUNT.
           PERFORM CALCULATE-AVERAGES.
           PERFORM STATISTICS.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       INPUT-STUDENT-DATA.
           DISPLAY '學生' WS-I '姓名: '.
           ACCEPT WS-NAME(WS-I).
           PERFORM INPUT-SCORES VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-SUBJECT-COUNT.
       
       INPUT-SCORES.
           DISPLAY '第' WS-J '科成績: '.
           ACCEPT WS-SCORES(WS-I, WS-J).
       
       CALCULATE-AVERAGES.
           MOVE 1 TO WS-I.
           PERFORM UNTIL WS-I > WS-STUDENT-COUNT
               MOVE 0 TO WS-TEMP-SCORE
               MOVE 1 TO WS-J
               PERFORM UNTIL WS-J > WS-SUBJECT-COUNT
                   ADD WS-SCORES(WS-I, WS-J) TO WS-TEMP-SCORE
                   ADD 1 TO WS-J
               END-PERFORM
               COMPUTE WS-AVG(WS-I) = WS-TEMP-SCORE / WS-SUBJECT-COUNT
               ADD WS-AVG(WS-I) TO WS-TOTAL-SUM
               ADD 1 TO WS-I
           END-PERFORM
           COMPUTE WS-CLASS-AVG = WS-TOTAL-SUM / WS-STUDENT-COUNT.
       
       STATISTICS.
           MOVE 1 TO WS-I.
           PERFORM UNTIL WS-I > WS-STUDENT-COUNT
               IF WS-AVG(WS-I) >= 90
                   ADD 1 TO WS-A-COUNT
               ELSE IF WS-AVG(WS-I) >= 80
                   ADD 1 TO WS-B-COUNT
               ELSE IF WS-AVG(WS-I) >= 70
                   ADD 1 TO WS-C-COUNT
               ELSE IF WS-AVG(WS-I) >= 60
                   ADD 1 TO WS-D-COUNT
               ELSE
                   ADD 1 TO WS-F-COUNT
               END-IF
               IF WS-AVG(WS-I) > WS-HIGHEST
                   MOVE WS-AVG(WS-I) TO WS-HIGHEST
               END-IF
               IF WS-AVG(WS-I) < WS-LOWEST
                   MOVE WS-AVG(WS-I) TO WS-LOWEST
               END-IF
               ADD 1 TO WS-I
           END-PERFORM.
       
       DISPLAY-RESULTS.
           DISPLAY '--- 成績統計報表 ---'.
           DISPLAY '全班平均: ' WS-CLASS-AVG.
           DISPLAY 'A級人數(90~100): ' WS-A-COUNT.
           DISPLAY 'B級人數(80~89): ' WS-B-COUNT.
           DISPLAY 'C級人數(70~79): ' WS-C-COUNT.
           DISPLAY 'D級人數(60~69): ' WS-D-COUNT.
           DISPLAY 'F級人數(0~59): ' WS-F-COUNT.
           DISPLAY '最高平均分: ' WS-HIGHEST.
           DISPLAY '最低平均分: ' WS-LOWEST.
           DISPLAY '-------------------'.
```

---

## 📝 本週小結

- 本週學習了COBOL的流程控制語法，包括IF-ELSE、EVALUATE、PERFORM等結構。
- 熟悉了條件判斷、邏輯運算、巢狀結構與多重選擇的寫法。
- 掌握了各種迴圈控制方式，能夠設計重複性與條件性流程。
- 學會了程式模組化設計，提升程式的可讀性與維護性。
- 透過實例練習，能夠將所學應用於實際問題解決。

---

## 📌 課後練習

1. 請設計一個COBOL程式，輸入10位員工的工號與薪資，計算平均薪資並找出最高與最低薪資。
2. 修改「學生成績管理系統」，增加每科最高分與最低分的統計功能。
3. 嘗試將IF-ELSE巢狀結構改寫為EVALUATE結構，提升程式可讀性。

---