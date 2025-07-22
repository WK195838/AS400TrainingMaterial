# 第十週：效能優化 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解AS/400系統效能分析的方法和工具
- 掌握程式碼層級的效能優化技術
- 熟練使用系統監控工具進行效能診斷
- 設計高效的資料存取策略和索引機制
- 實作記憶體管理和I/O優化技術
- 建立完整的效能監控和調校流程

---

## 📊 第一節：效能分析基礎

### 1.1 AS/400效能架構

#### 1.1.1 系統效能層次模型

```
AS/400 效能分析層次架構：
┌─────────────────────────────────────────┐
│ 應用程式層 (Application Layer)          │
│ ┌─────────────────────────────────────┐ │
│ │ COBOL程式效能                      │ │
│ │ - 演算法效率                       │ │
│ │ - 迴圈優化                         │ │
│ │ - 記憶體使用                       │ │
│ │ - 函數呼叫開銷                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 資料存取
┌─────────────────────────────────────────┐
│ 資料存取層 (Data Access Layer)          │
│ ┌─────────────────────────────────────┐ │
│ │ 檔案和資料庫存取                   │ │
│ │ - SQL查詢優化                      │ │
│ │ - 索引使用策略                     │ │
│ │ - 記錄鎖定機制                     │ │
│ │ - 資料緩存管理                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 系統呼叫
┌─────────────────────────────────────────┐
│ 系統服務層 (System Service Layer)      │
│ ┌─────────────────────────────────────┐ │
│ │ OS/400系統服務                     │ │
│ │ - 作業調度                         │ │
│ │ - 記憶體管理                       │ │
│ │ - I/O處理                          │ │
│ │ - 網路通訊                         │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 硬體存取
┌─────────────────────────────────────────┐
│ 硬體資源層 (Hardware Resource Layer)   │
│ ┌─────────────────────────────────────┐ │
│ │ 實體硬體資源                       │ │
│ │ - CPU使用率                        │ │
│ │ - 記憶體容量                       │ │
│ │ - 磁碟I/O                          │ │
│ │ - 網路頻寬                         │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

#### 1.1.2 效能指標體系

```cobol
      *    AS/400 效能監控指標分類：
      *
      *    1. 系統層級指標 (System Level Metrics)
      *       - CPU利用率和等待時間
      *       - 記憶體使用率和分頁活動
      *       - 磁碟I/O頻率和回應時間
      *       - 網路流量和延遲
      *
      *    2. 應用層級指標 (Application Level Metrics)
      *       - 程式執行時間
      *       - 資料庫查詢效能
      *       - 交易處理量
      *       - 使用者回應時間
      *
      *    3. 資源使用指標 (Resource Usage Metrics)
      *       - 檔案存取次數
      *       - 記錄鎖定統計
      *       - 快取命中率
      *       - 併發使用者數量
      *
      *    4. 商業效能指標 (Business Performance Metrics)
      *       - 交易完成率
      *       - 系統可用性
      *       - 服務品質等級
      *       - 使用者滿意度
```

### 1.2 效能分析工具

#### 1.2.1 系統監控程式

```cobol
      *****************************************************************
      * 程式名稱：PERFMON                                            *
      * 程式功能：系統效能監控程式                                   *
      * 作者：效能調校專家                                           *
      * 日期：2024/02/15                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFMON.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    系統資訊結構
       01  WS-SYSTEM-INFO.
           05  WS-SYS-NAME         PIC X(8).
           05  WS-SYS-TYPE         PIC X(4).
           05  WS-SYS-MODEL        PIC X(4).
           05  WS-CPU-COUNT        PIC 9(3).
           05  WS-MEMORY-SIZE      PIC 9(8) COMP.
           
      *    效能統計資料
       01  WS-PERFORMANCE-STATS.
           05  WS-CPU-UTILIZATION.
               10  WS-CPU-TOTAL    PIC 9(3)V99.
               10  WS-CPU-USER     PIC 9(3)V99.
               10  WS-CPU-SYSTEM   PIC 9(3)V99.
               10  WS-CPU-WAIT     PIC 9(3)V99.
               10  WS-CPU-IDLE     PIC 9(3)V99.
               
           05  WS-MEMORY-USAGE.
               10  WS-MEM-TOTAL    PIC 9(10) COMP.
               10  WS-MEM-USED     PIC 9(10) COMP.
               10  WS-MEM-FREE     PIC 9(10) COMP.
               10  WS-MEM-PERCENT  PIC 9(3)V99.
               
           05  WS-DISK-ACTIVITY.
               10  WS-DISK-READS   PIC 9(10) COMP.
               10  WS-DISK-WRITES  PIC 9(10) COMP.
               10  WS-DISK-READ-TIME PIC 9(8)V99 COMP.
               10  WS-DISK-WRITE-TIME PIC 9(8)V99 COMP.
               
           05  WS-JOB-STATISTICS.
               10  WS-ACTIVE-JOBS  PIC 9(5).
               10  WS-WAITING-JOBS PIC 9(5).
               10  WS-TOTAL-JOBS   PIC 9(5).
               
      *    監控參數
       01  WS-MONITOR-CONTROL.
           05  WS-MONITOR-INTERVAL PIC 9(3) VALUE 60.
           05  WS-MONITOR-DURATION PIC 9(5) VALUE 3600.
           05  WS-SAMPLE-COUNT     PIC 9(5) VALUE 0.
           05  WS-CONTINUE-MONITOR PIC X.
               88  CONTINUE-MONITORING     VALUE 'Y'.
               88  STOP-MONITORING         VALUE 'N'.
               
      *    報告控制
       01  WS-REPORT-CONTROL.
           05  WS-REPORT-FILE      PIC X(21) VALUE '/tmp/perfmon.rpt'.
           05  WS-TIMESTAMP        PIC X(26).
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-MONITORING
           PERFORM COLLECT-BASELINE
           PERFORM CONTINUOUS-MONITORING
           PERFORM GENERATE-REPORT.
           STOP RUN.
           
       INITIALIZE-MONITORING.
           DISPLAY 'Initializing Performance Monitoring...'.
           
           PERFORM GET-SYSTEM-INFORMATION.
           PERFORM SETUP-MONITORING-PARAMETERS.
           
           SET CONTINUE-MONITORING TO TRUE.
           MOVE 0 TO WS-SAMPLE-COUNT.
           
           DISPLAY 'System: ' WS-SYS-NAME ' Model: ' WS-SYS-MODEL.
           DISPLAY 'CPUs: ' WS-CPU-COUNT ' Memory: ' WS-MEMORY-SIZE ' MB'.
           DISPLAY 'Monitor interval: ' WS-MONITOR-INTERVAL ' seconds'.
           
       GET-SYSTEM-INFORMATION.
      *    使用系統API取得硬體資訊
           CALL 'QSZRTVPR' USING
               'SYSNAME'
               WS-SYS-NAME.
               
           CALL 'QSZRTVPR' USING
               'QMODEL'
               WS-SYS-MODEL.
               
           CALL 'QSZRTVPR' USING
               'QPRCFEAT'
               WS-CPU-COUNT.
               
           CALL 'QSZRTVPR' USING
               'QSTGLMT'
               WS-MEMORY-SIZE.
               
       SETUP-MONITORING-PARAMETERS.
      *    可以從參數檔案或使用者輸入設定監控參數
           DISPLAY 'Monitor interval (seconds): '.
           ACCEPT WS-MONITOR-INTERVAL.
           
           DISPLAY 'Monitor duration (seconds): '.
           ACCEPT WS-MONITOR-DURATION.
           
       COLLECT-BASELINE.
           DISPLAY 'Collecting baseline performance data...'.
           
           PERFORM GET-CURRENT-STATS.
           
           DISPLAY 'Baseline collected:'.
           DISPLAY '  CPU Usage: ' WS-CPU-TOTAL '%'.
           DISPLAY '  Memory Usage: ' WS-MEM-PERCENT '%'.
           DISPLAY '  Active Jobs: ' WS-ACTIVE-JOBS.
           
       CONTINUOUS-MONITORING.
           DISPLAY 'Starting continuous monitoring...'.
           
           PERFORM UNTIL STOP-MONITORING
               PERFORM GET-CURRENT-STATS
               PERFORM ANALYZE-PERFORMANCE
               PERFORM LOG-STATISTICS
               
               ADD 1 TO WS-SAMPLE-COUNT
               
               IF WS-SAMPLE-COUNT * WS-MONITOR-INTERVAL >= WS-MONITOR-DURATION
                   SET STOP-MONITORING TO TRUE
               ELSE
                   CALL 'QSLEEP' USING WS-MONITOR-INTERVAL * 1000
               END-IF
           END-PERFORM.
           
       GET-CURRENT-STATS.
      *    收集當前系統統計資料
           PERFORM GET-CPU-STATISTICS.
           PERFORM GET-MEMORY-STATISTICS.
           PERFORM GET-DISK-STATISTICS.
           PERFORM GET-JOB-STATISTICS.
           
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP.
           
       GET-CPU-STATISTICS.
      *    使用QSYS2系統服務取得CPU統計
           EXEC SQL
               SELECT 
                   AVERAGE_CPU_UTILIZATION,
                   CURRENT_CPU_CAPACITY,
                   CPU_CONFIGURED,
                   ELAPSED_TIME
               INTO 
                   :WS-CPU-TOTAL,
                   :WS-CPU-USER,
                   :WS-CPU-COUNT,
                   :WS-CPU-WAIT
               FROM QSYS2.SYSTEM_STATUS_INFO
           END-EXEC.
           
           COMPUTE WS-CPU-IDLE = 100 - WS-CPU-TOTAL.
           
       GET-MEMORY-STATISTICS.
      *    取得記憶體使用統計
           EXEC SQL
               SELECT 
                   MAIN_STORAGE_SIZE,
                   CURRENT_UNPROTECTED_USED,
                   (CURRENT_UNPROTECTED_USED * 100.0) / MAIN_STORAGE_SIZE
               INTO 
                   :WS-MEM-TOTAL,
                   :WS-MEM-USED,
                   :WS-MEM-PERCENT
               FROM QSYS2.SYSTEM_STATUS_INFO
           END-EXEC.
           
           COMPUTE WS-MEM-FREE = WS-MEM-TOTAL - WS-MEM-USED.
           
       GET-DISK-STATISTICS.
      *    取得磁碟I/O統計
           EXEC SQL
               SELECT 
                   SUM(DISK_READS),
                   SUM(DISK_WRITES),
                   AVG(DISK_READ_TIME),
                   AVG(DISK_WRITE_TIME)
               INTO 
                   :WS-DISK-READS,
                   :WS-DISK-WRITES,
                   :WS-DISK-READ-TIME,
                   :WS-DISK-WRITE-TIME
               FROM QSYS2.DISK_STATUS_INFO
           END-EXEC.
           
       GET-JOB-STATISTICS.
      *    取得作業統計
           EXEC SQL
               SELECT 
                   COUNT(*) FILTER (WHERE JOB_STATUS = 'ACTIVE'),
                   COUNT(*) FILTER (WHERE JOB_STATUS = 'WAIT'),
                   COUNT(*)
               INTO 
                   :WS-ACTIVE-JOBS,
                   :WS-WAITING-JOBS,
                   :WS-TOTAL-JOBS
               FROM QSYS2.ACTIVE_JOB_INFO
               WHERE JOB_TYPE <> 'SYS'
           END-EXEC.
           
       ANALYZE-PERFORMANCE.
      *    分析效能趨勢和異常
           PERFORM CHECK-CPU-THRESHOLD.
           PERFORM CHECK-MEMORY-THRESHOLD.
           PERFORM CHECK-DISK-THRESHOLD.
           PERFORM CHECK-JOB-THRESHOLD.
           
       CHECK-CPU-THRESHOLD.
           IF WS-CPU-TOTAL > 80
               DISPLAY 'WARNING: High CPU utilization: ' WS-CPU-TOTAL '%'
               PERFORM RECOMMEND-CPU-OPTIMIZATION
           END-IF.
           
           IF WS-CPU-WAIT > 30
               DISPLAY 'WARNING: High CPU wait time: ' WS-CPU-WAIT '%'
               DISPLAY 'Possible I/O bottleneck detected'
           END-IF.
           
       CHECK-MEMORY-THRESHOLD.
           IF WS-MEM-PERCENT > 85
               DISPLAY 'WARNING: High memory usage: ' WS-MEM-PERCENT '%'
               PERFORM RECOMMEND-MEMORY-OPTIMIZATION
           END-IF.
           
       CHECK-DISK-THRESHOLD.
           IF WS-DISK-READ-TIME > 50 OR WS-DISK-WRITE-TIME > 50
               DISPLAY 'WARNING: High disk response time'
               DISPLAY 'Read time: ' WS-DISK-READ-TIME ' ms'
               DISPLAY 'Write time: ' WS-DISK-WRITE-TIME ' ms'
               PERFORM RECOMMEND-DISK-OPTIMIZATION
           END-IF.
           
       CHECK-JOB-THRESHOLD.
           IF WS-WAITING-JOBS > WS-ACTIVE-JOBS
               DISPLAY 'WARNING: More jobs waiting than active'
               DISPLAY 'Active: ' WS-ACTIVE-JOBS ' Waiting: ' WS-WAITING-JOBS
           END-IF.
           
       RECOMMEND-CPU-OPTIMIZATION.
           DISPLAY 'CPU Optimization Recommendations:'.
           DISPLAY '1. Check for runaway jobs'.
           DISPLAY '2. Review program efficiency'.
           DISPLAY '3. Consider CPU upgrade'.
           DISPLAY '4. Implement workload balancing'.
           
       RECOMMEND-MEMORY-OPTIMIZATION.
           DISPLAY 'Memory Optimization Recommendations:'.
           DISPLAY '1. Increase main storage'.
           DISPLAY '2. Optimize program memory usage'.
           DISPLAY '3. Review pool sizes'.
           DISPLAY '4. Consider memory compression'.
           
       RECOMMEND-DISK-OPTIMIZATION.
           DISPLAY 'Disk Optimization Recommendations:'.
           DISPLAY '1. Check disk configuration'.
           DISPLAY '2. Optimize file placement'.
           DISPLAY '3. Review index usage'.
           DISPLAY '4. Consider SSD upgrade'.
           
       LOG-STATISTICS.
      *    記錄統計資料到檔案
           STRING 
               WS-TIMESTAMP ','
               WS-CPU-TOTAL ','
               WS-MEM-PERCENT ','
               WS-DISK-READS ','
               WS-DISK-WRITES ','
               WS-ACTIVE-JOBS ','
               WS-WAITING-JOBS
               DELIMITED BY SIZE
               INTO WS-LOG-RECORD.
               
      *    寫入記錄檔
           PERFORM WRITE-LOG-RECORD.
           
       WRITE-LOG-RECORD.
      *    這裡應該寫入實際的記錄檔
           DISPLAY 'Sample ' WS-SAMPLE-COUNT ': ' 
                   'CPU=' WS-CPU-TOTAL '% '
                   'MEM=' WS-MEM-PERCENT '% '
                   'JOBS=' WS-ACTIVE-JOBS.
                   
       GENERATE-REPORT.
           DISPLAY ' '.
           DISPLAY '========== Performance Summary =========='.
           DISPLAY 'Monitoring Duration: ' WS-MONITOR-DURATION ' seconds'.
           DISPLAY 'Total Samples: ' WS-SAMPLE-COUNT.
           DISPLAY ' '.
           DISPLAY 'Final Statistics:'.
           DISPLAY '  CPU Utilization: ' WS-CPU-TOTAL '%'.
           DISPLAY '  Memory Usage: ' WS-MEM-PERCENT '%'.
           DISPLAY '  Active Jobs: ' WS-ACTIVE-JOBS.
           DISPLAY '  Disk Reads: ' WS-DISK-READS.
           DISPLAY '  Disk Writes: ' WS-DISK-WRITES.
           DISPLAY '========================================'.
```

---

## ⚡ 第二節：程式碼優化技術

### 2.1 演算法和資料結構優化

#### 2.1.1 高效的資料處理演算法

```cobol
      *****************************************************************
      * 程式名稱：ALGOPT                                             *
      * 程式功能：演算法優化示範                                     *
      * 作者：效能優化專家                                           *
      * 日期：2024/02/15                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALGOPT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    大量資料處理結構
       01  WS-LARGE-DATASET.
           05  WS-RECORD-COUNT     PIC 9(6) VALUE 100000.
           05  WS-EMPLOYEE-TABLE.
               10  WS-EMP-ENTRY OCCURS 100000 TIMES 
                                INDEXED BY WS-EMP-IDX.
                   15  WS-EMP-ID       PIC X(6).
                   15  WS-EMP-NAME     PIC X(30).
                   15  WS-EMP-SALARY   PIC 9(8)V99 COMP-3.
                   15  WS-EMP-DEPT     PIC X(4).
                   15  WS-EMP-ACTIVE   PIC X.
                   
      *    索引和查找表
       01  WS-OPTIMIZATION-STRUCTURES.
           05  WS-SALARY-INDEX OCCURS 100000 TIMES 
                               INDEXED BY WS-SAL-IDX.
               10  WS-SALARY-VALUE     PIC 9(8)V99 COMP-3.
               10  WS-SALARY-EMP-IDX   PIC 9(6) COMP.
               
           05  WS-DEPT-LOOKUP.
               10  WS-DEPT-ENTRY OCCURS 20 TIMES 
                                 INDEXED BY WS-DEPT-IDX.
                   15  WS-DEPT-CODE    PIC X(4).
                   15  WS-DEPT-COUNT   PIC 9(6) COMP.
                   15  WS-DEPT-TOTAL   PIC 9(10)V99 COMP-3.
                   
      *    效能測試變數
       01  WS-PERFORMANCE-TEST.
           05  WS-START-TIME       PIC 9(18) COMP.
           05  WS-END-TIME         PIC 9(18) COMP.
           05  WS-ELAPSED-TIME     PIC 9(10)V99 COMP.
           05  WS-TEST-ITERATIONS  PIC 9(6).
           
      *    比較結果
       01  WS-COMPARISON-RESULTS.
           05  WS-NAIVE-TIME       PIC 9(8)V99 COMP.
           05  WS-OPTIMIZED-TIME   PIC 9(8)V99 COMP.
           05  WS-IMPROVEMENT      PIC 9(5)V99 COMP.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-TEST-DATA
           PERFORM DEMONSTRATE-SEARCH-OPTIMIZATION
           PERFORM DEMONSTRATE-SORT-OPTIMIZATION
           PERFORM DEMONSTRATE-AGGREGATION-OPTIMIZATION
           PERFORM DEMONSTRATE-MEMORY-OPTIMIZATION.
           STOP RUN.
           
       INITIALIZE-TEST-DATA.
           DISPLAY 'Initializing test data (100,000 records)...'.
           
           PERFORM VARYING WS-EMP-IDX FROM 1 BY 1 
                   UNTIL WS-EMP-IDX > WS-RECORD-COUNT
               
      *        產生測試資料
               STRING 'E' WS-EMP-IDX 
                      DELIMITED BY SIZE 
                      INTO WS-EMP-ID(WS-EMP-IDX)
               
               STRING 'Employee ' WS-EMP-IDX
                      DELIMITED BY SIZE
                      INTO WS-EMP-NAME(WS-EMP-IDX)
                      
               COMPUTE WS-EMP-SALARY(WS-EMP-IDX) = 
                   30000 + (FUNCTION MOD(WS-EMP-IDX, 70000))
                   
               COMPUTE WS-DEPT-CALC = FUNCTION MOD(WS-EMP-IDX, 4) + 1
               EVALUATE WS-DEPT-CALC
                   WHEN 1 MOVE 'IT01' TO WS-EMP-DEPT(WS-EMP-IDX)
                   WHEN 2 MOVE 'HR01' TO WS-EMP-DEPT(WS-EMP-IDX)
                   WHEN 3 MOVE 'FN01' TO WS-EMP-DEPT(WS-EMP-IDX)
                   WHEN 4 MOVE 'SL01' TO WS-EMP-DEPT(WS-EMP-IDX)
               END-EVALUATE
               
               MOVE 'Y' TO WS-EMP-ACTIVE(WS-EMP-IDX)
           END-PERFORM.
           
           DISPLAY 'Test data initialized successfully.'.
           
       DEMONSTRATE-SEARCH-OPTIMIZATION.
           DISPLAY ' '.
           DISPLAY '=== Search Algorithm Optimization ==='.
           
      *    測試線性搜尋 vs 二分搜尋
           PERFORM TEST-LINEAR-SEARCH.
           PERFORM BUILD-SEARCH-INDEX.
           PERFORM TEST-BINARY-SEARCH.
           PERFORM COMPARE-SEARCH-RESULTS.
           
       TEST-LINEAR-SEARCH.
           DISPLAY 'Testing linear search...'.
           
           MOVE FUNCTION CURRENT-DATE TO WS-START-TIME.
           
           MOVE 1000 TO WS-TEST-ITERATIONS.
           PERFORM WS-TEST-ITERATIONS TIMES
               PERFORM LINEAR-SEARCH-SAMPLE
           END-PERFORM.
           
           MOVE FUNCTION CURRENT-DATE TO WS-END-TIME.
           COMPUTE WS-NAIVE-TIME = WS-END-TIME - WS-START-TIME.
           
           DISPLAY 'Linear search completed in: ' WS-NAIVE-TIME ' ms'.
           
       LINEAR-SEARCH-SAMPLE.
      *    搜尋隨機員工
           COMPUTE WS-SEARCH-TARGET = FUNCTION MOD(FUNCTION RANDOM, WS-RECORD-COUNT) + 1.
           
           PERFORM VARYING WS-EMP-IDX FROM 1 BY 1 
                   UNTIL WS-EMP-IDX > WS-RECORD-COUNT
               IF WS-EMP-SALARY(WS-EMP-IDX) > 80000
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
       BUILD-SEARCH-INDEX.
           DISPLAY 'Building salary index for binary search...'.
           
      *    複製薪資資料到索引陣列
           PERFORM VARYING WS-EMP-IDX FROM 1 BY 1 
                   UNTIL WS-EMP-IDX > WS-RECORD-COUNT
               MOVE WS-EMP-SALARY(WS-EMP-IDX) TO WS-SALARY-VALUE(WS-EMP-IDX)
               MOVE WS-EMP-IDX TO WS-SALARY-EMP-IDX(WS-EMP-IDX)
           END-PERFORM.
           
      *    排序索引陣列（使用快速排序演算法）
           PERFORM QUICK-SORT-SALARY 
               USING 1 WS-RECORD-COUNT.
               
       QUICK-SORT-SALARY USING WS-LOW WS-HIGH.
      *    簡化的快速排序實作
           IF WS-LOW < WS-HIGH
               PERFORM PARTITION-SALARY USING WS-LOW WS-HIGH 
                                       RETURNING WS-PIVOT
               PERFORM QUICK-SORT-SALARY USING WS-LOW WS-PIVOT - 1
               PERFORM QUICK-SORT-SALARY USING WS-PIVOT + 1 WS-HIGH
           END-IF.
           
       TEST-BINARY-SEARCH.
           DISPLAY 'Testing binary search...'.
           
           MOVE FUNCTION CURRENT-DATE TO WS-START-TIME.
           
           PERFORM WS-TEST-ITERATIONS TIMES
               PERFORM BINARY-SEARCH-SAMPLE
           END-PERFORM.
           
           MOVE FUNCTION CURRENT-DATE TO WS-END-TIME.
           COMPUTE WS-OPTIMIZED-TIME = WS-END-TIME - WS-START-TIME.
           
           DISPLAY 'Binary search completed in: ' WS-OPTIMIZED-TIME ' ms'.
           
       BINARY-SEARCH-SAMPLE.
      *    使用二分搜尋法搜尋薪資 > 80000的員工
           MOVE 1 TO WS-LOW.
           MOVE WS-RECORD-COUNT TO WS-HIGH.
           
           PERFORM UNTIL WS-LOW > WS-HIGH
               COMPUTE WS-MID = (WS-LOW + WS-HIGH) / 2
               
               IF WS-SALARY-VALUE(WS-MID) > 80000
                   MOVE WS-MID TO WS-HIGH
                   SUBTRACT 1 FROM WS-HIGH
               ELSE
                   MOVE WS-MID TO WS-LOW
                   ADD 1 TO WS-LOW
               END-IF
           END-PERFORM.
           
       COMPARE-SEARCH-RESULTS.
           COMPUTE WS-IMPROVEMENT = 
               ((WS-NAIVE-TIME - WS-OPTIMIZED-TIME) / WS-NAIVE-TIME) * 100.
               
           DISPLAY 'Search Optimization Results:'.
           DISPLAY '  Linear Search: ' WS-NAIVE-TIME ' ms'.
           DISPLAY '  Binary Search: ' WS-OPTIMIZED-TIME ' ms'.
           DISPLAY '  Improvement: ' WS-IMPROVEMENT '%'.
           
       DEMONSTRATE-SORT-OPTIMIZATION.
           DISPLAY ' '.
           DISPLAY '=== Sort Algorithm Optimization ==='.
           
      *    比較不同排序演算法的效能
           PERFORM TEST-BUBBLE-SORT.
           PERFORM TEST-QUICK-SORT.
           PERFORM COMPARE-SORT-RESULTS.
           
       DEMONSTRATE-AGGREGATION-OPTIMIZATION.
           DISPLAY ' '.
           DISPLAY '=== Aggregation Optimization ==='.
           
      *    比較簡單累加 vs 批次處理
           PERFORM TEST-SIMPLE-AGGREGATION.
           PERFORM TEST-BATCH-AGGREGATION.
           PERFORM COMPARE-AGGREGATION-RESULTS.
           
       TEST-SIMPLE-AGGREGATION.
           DISPLAY 'Testing simple aggregation...'.
           
           MOVE FUNCTION CURRENT-DATE TO WS-START-TIME.
           
      *    逐一處理每筆記錄
           MOVE 0 TO WS-TOTAL-SALARY.
           PERFORM VARYING WS-EMP-IDX FROM 1 BY 1 
                   UNTIL WS-EMP-IDX > WS-RECORD-COUNT
               ADD WS-EMP-SALARY(WS-EMP-IDX) TO WS-TOTAL-SALARY
           END-PERFORM.
           
           MOVE FUNCTION CURRENT-DATE TO WS-END-TIME.
           COMPUTE WS-NAIVE-TIME = WS-END-TIME - WS-START-TIME.
           
           DISPLAY 'Simple aggregation: ' WS-NAIVE-TIME ' ms'.
           DISPLAY 'Total salary: ' WS-TOTAL-SALARY.
           
       TEST-BATCH-AGGREGATION.
           DISPLAY 'Testing batch aggregation...'.
           
           MOVE FUNCTION CURRENT-DATE TO WS-START-