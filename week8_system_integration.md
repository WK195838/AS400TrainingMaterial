# 第八週：系統整合 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解企業級系統架構和整合模式
- 掌握AS/400系統間通訊和資料交換技術
- 熟練使用資料佇列、使用者空間等IPC機制
- 設計和實作批次處理和線上處理系統
- 開發外部系統整合介面和API
- 建立可擴展的企業應用架構

---

## 🏗️ 第一節：企業系統架構概論

### 1.1 多層式系統架構

#### 1.1.1 三層式架構模型

```
企業級三層式架構：
┌─────────────────────────────────────────┐
│ 展示層 (Presentation Tier)              │
│ ┌─────────────────────────────────────┐ │
│ │ 使用者介面 (User Interface)         │ │
│ │ - 5250終端機界面                   │ │
│ │ - Web瀏覽器界面                    │ │
│ │ - 行動應用程式                     │ │ 
│ │ - 胖客戶端應用                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 通訊協定 (HTTP/HTTPS/API)
┌─────────────────────────────────────────┐
│ 商業邏輯層 (Business Logic Tier)        │
│ ┌─────────────────────────────────────┐ │
│ │ 應用伺服器 (Application Server)     │ │
│ │ - COBOL商業程式                    │ │
│ │ - Java企業應用                     │ │
│ │ - Web Services                     │ │
│ │ - 工作流程引擎                     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↕ 資料存取協定 (SQL/ODBC/JDBC)
┌─────────────────────────────────────────┐
│ 資料層 (Data Tier)                     │
│ ┌─────────────────────────────────────┐ │
│ │ 資料庫系統 (Database System)        │ │
│ │ - DB2/400主資料庫                  │ │
│ │ - 檔案系統                         │ │
│ │ - 外部資料庫                       │ │
│ │ - 資料倉儲                         │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

#### 1.1.2 微服務架構模式

```cobol
      *    微服務架構在AS/400環境中的實現模式：
      *
      *    1. 服務導向架構 (SOA)
      *       - 獨立的業務功能模組
      *       - 標準化的服務介面
      *       - 鬆散耦合的系統設計
      *
      *    2. 分散式處理
      *       - 各服務獨立部署
      *       - 水平擴展能力
      *       - 故障隔離機制
      *
      *    3. 資料管理策略
      *       - 每個服務有專屬資料庫
      *       - 最終一致性模型
      *       - 分散式交易處理
      *
      *    4. 通訊機制
      *       - RESTful API
      *       - 訊息佇列
      *       - 事件驅動架構
```

### 1.2 AS/400系統整合特色

#### 1.2.1 內建整合能力

```cobol
      *    AS/400系統整合的核心優勢：
      *
      *    1. 物件導向設計
      *       - 統一的物件管理
      *       - 一致的安全模型
      *       - 標準化的API介面
      *
      *    2. 多重存取方式
      *       - 原生檔案存取
      *       - SQL資料庫存取
      *       - ODBC/JDBC連接
      *       - Web Services支援
      *
      *    3. 強大的通訊能力
      *       - TCP/IP網路支援
      *       - SNA網路協定
      *       - 電子郵件整合
      *       - FTP檔案傳輸
      *
      *    4. 企業級功能
      *       - 高可用性叢集
      *       - 自動故障轉移
      *       - 負載平衡
      *       - 災難復原
```

---

## 🔄 第二節：程式間通訊 (IPC)

### 2.1 資料佇列 (Data Queue)

#### 2.1.1 資料佇列基本概念

```cobol
      *    資料佇列是AS/400系統中程式間通訊的重要機制
      *    
      *    特色：
      *    - FIFO (先進先出) 或 LIFO (後進先出) 處理
      *    - 支援多個程式同時存取
      *    - 可設定最大項目數和項目大小
      *    - 支援等待和非等待模式
      *    - 自動鎖定機制
      
      *    資料佇列類型：
      *    - 標準資料佇列 (Standard Data Queue)
      *    - 鍵值資料佇列 (Keyed Data Queue)  
      *    - DDM資料佇列 (分散式資料佇列)
```

#### 2.1.2 建立和使用資料佇列

```cobol
      *****************************************************************
      * 程式名稱：DQDEMO                                             *
      * 程式功能：資料佇列示範程式                                   *
      * 作者：系統整合工程師                                         *
      * 日期：2024/02/05                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DQDEMO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    資料佇列相關變數
       01  WS-DATA-QUEUE-PARAMS.
           05  WS-DQ-NAME          PIC X(10) VALUE 'MSGQUEUE'.
           05  WS-DQ-LIBRARY       PIC X(10) VALUE 'QGPL'.
           05  WS-DQ-MESSAGE       PIC X(100).
           05  WS-DQ-MSG-LENGTH    PIC 9(5) COMP VALUE 100.
           05  WS-DQ-WAIT-TIME     PIC 9(5) COMP VALUE -1.
           05  WS-DQ-KEY           PIC X(10).
           05  WS-DQ-KEY-LENGTH    PIC 9(3) COMP VALUE 10.
           05  WS-DQ-SENDER-INFO   PIC X(36).
           
      *    錯誤處理
       01  WS-ERROR-HANDLING.
           05  WS-ERROR-CODE.
               10  WS-ERROR-PROVIDED   PIC 9(9) COMP VALUE 116.
               10  WS-ERROR-AVAILABLE  PIC 9(9) COMP.
               10  WS-ERROR-MSGID      PIC X(7).
               10  WS-ERROR-RESERVED   PIC X.
               10  WS-ERROR-MSGDATA    PIC X(100).
               
      *    操作模式
       01  WS-OPERATION-MODE       PIC X(10).
           88  SEND-MODE                   VALUE 'SEND'.
           88  RECEIVE-MODE                VALUE 'RECEIVE'.
           88  PEEK-MODE                   VALUE 'PEEK'.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM DEMONSTRATE-DATA-QUEUE
           STOP RUN.
           
       INITIALIZE-PROGRAM.
           DISPLAY 'Data Queue Demonstration Program'.
           PERFORM CREATE-DATA-QUEUE.
           
       CREATE-DATA-QUEUE.
      *    建立標準資料佇列
           CALL 'QCRTDTAQ' USING
               WS-DQ-NAME
               WS-DQ-LIBRARY
               'D'              *標準資料佇列
               100              *項目長度
               '*USE'           *權限
               'N'              *不包含發送者資訊
               'N'              *不是FIFO
               0                *最大項目數(0=無限制)
               0                *初始項目數
               'Message Queue for Demo'  *描述
               WS-ERROR-CODE.
               
           IF WS-ERROR-AVAILABLE > 0
               DISPLAY 'Error creating data queue: ' WS-ERROR-MSGID
           ELSE
               DISPLAY 'Data queue created successfully'
           END-IF.
           
       DEMONSTRATE-DATA-QUEUE.
           PERFORM SEND-MESSAGES
           PERFORM RECEIVE-MESSAGES.
           
       SEND-MESSAGES.
           DISPLAY ' '.
           DISPLAY '=== Sending Messages ==='.
           
           MOVE 'Hello from COBOL Program 1' TO WS-DQ-MESSAGE.
           PERFORM SEND-TO-DATA-QUEUE.
           
           MOVE 'Processing Order #12345' TO WS-DQ-MESSAGE.
           PERFORM SEND-TO-DATA-QUEUE.
           
           MOVE 'System Maintenance Alert' TO WS-DQ-MESSAGE.
           PERFORM SEND-TO-DATA-QUEUE.
           
       SEND-TO-DATA-QUEUE.
           CALL 'QSNDDTAQ' USING
               WS-DQ-NAME
               WS-DQ-LIBRARY
               WS-DQ-MSG-LENGTH
               WS-DQ-MESSAGE.
               
           DISPLAY 'Message sent: ' WS-DQ-MESSAGE.
           
       RECEIVE-MESSAGES.
           DISPLAY ' '.
           DISPLAY '=== Receiving Messages ==='.
           
           PERFORM RECEIVE-FROM-DATA-QUEUE 5 TIMES.
           
       RECEIVE-FROM-DATA-QUEUE.
           MOVE SPACES TO WS-DQ-MESSAGE.
           
           CALL 'QRCVDTAQ' USING
               WS-DQ-NAME
               WS-DQ-LIBRARY
               WS-DQ-MSG-LENGTH
               WS-DQ-MESSAGE
               WS-DQ-WAIT-TIME
               WS-DQ-SENDER-INFO.
               
           IF WS-DQ-MSG-LENGTH > 0
               DISPLAY 'Message received: ' WS-DQ-MESSAGE
               DISPLAY 'Message length: ' WS-DQ-MSG-LENGTH
           ELSE
               DISPLAY 'No more messages in queue'
           END-IF.
```

#### 2.1.3 鍵值資料佇列應用

```cobol
      *****************************************************************
      * 程式名稱：KEYDQ                                              *
      * 程式功能：鍵值資料佇列處理                                   *
      * 作者：系統整合工程師                                         *
      * 日期：2024/02/05                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KEYDQ.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    鍵值資料佇列參數
       01  WS-KEYED-DQ-PARAMS.
           05  WS-KDQ-NAME         PIC X(10) VALUE 'ORDQUEUE'.
           05  WS-KDQ-LIBRARY      PIC X(10) VALUE 'QGPL'.
           05  WS-KDQ-KEY-LENGTH   PIC 9(3) COMP VALUE 10.
           05  WS-KDQ-ENTRY-LENGTH PIC 9(5) COMP VALUE 200.
           
      *    訂單處理資料結構
       01  WS-ORDER-DATA.
           05  WS-ORDER-KEY        PIC X(10).
           05  WS-ORDER-ENTRY.
               10  WS-ORDER-ID     PIC X(10).
               10  WS-CUSTOMER-ID  PIC X(8).
               10  WS-PRODUCT-ID   PIC X(12).
               10  WS-QUANTITY     PIC 9(5).
               10  WS-UNIT-PRICE   PIC 9(7)V99.
               10  WS-ORDER-DATE   PIC X(10).
               10  WS-STATUS       PIC X(10).
               10  WS-PRIORITY     PIC 9.
               10  WS-NOTES        PIC X(120).
               
      *    搜尋參數
       01  WS-SEARCH-PARAMS.
           05  WS-SEARCH-ORDER     PIC X(2).
               88  SEARCH-EQUAL            VALUE 'EQ'.
               88  SEARCH-GREATER          VALUE 'GT'.
               88  SEARCH-LESS             VALUE 'LT'.
               88  SEARCH-GREATER-EQUAL    VALUE 'GE'.
               88  SEARCH-LESS-EQUAL       VALUE 'LE'.
           05  WS-SEARCH-KEY       PIC X(10).
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-KEYED-QUEUE
           PERFORM DEMONSTRATE-KEYED-OPERATIONS
           STOP RUN.
           
       INITIALIZE-KEYED-QUEUE.
           DISPLAY 'Creating Keyed Data Queue...'.
           
           CALL 'QCRTDTAQ' USING
               WS-KDQ-NAME
               WS-KDQ-LIBRARY
               'K'                      *鍵值資料佇列
               WS-KDQ-ENTRY-LENGTH
               '*USE'
               'N'
               'Y'                      *FIFO處理
               1000                     *最大項目數
               0
               'Order Processing Queue'
               WS-ERROR-CODE.
               
       DEMONSTRATE-KEYED-OPERATIONS.
           PERFORM ADD-SAMPLE-ORDERS
           PERFORM SEARCH-ORDERS
           PERFORM PROCESS-HIGH-PRIORITY-ORDERS.
           
       ADD-SAMPLE-ORDERS.
           DISPLAY ' '.
           DISPLAY '=== Adding Sample Orders ==='.
           
      *    高優先級訂單
           MOVE 'ORD0001' TO WS-ORDER-KEY.
           MOVE 'ORD0001' TO WS-ORDER-ID.
           MOVE 'CUST001' TO WS-CUSTOMER-ID.
           MOVE 'PROD12345' TO WS-PRODUCT-ID.
           MOVE 100 TO WS-QUANTITY.
           MOVE 250.00 TO WS-UNIT-PRICE.
           MOVE '2024-02-05' TO WS-ORDER-DATE.
           MOVE 'PENDING' TO WS-STATUS.
           MOVE 1 TO WS-PRIORITY.
           MOVE 'Urgent delivery required' TO WS-NOTES.
           PERFORM SEND-KEYED-ORDER.
           
      *    一般訂單
           MOVE 'ORD0002' TO WS-ORDER-KEY.
           MOVE 'ORD0002' TO WS-ORDER-ID.
           MOVE 'CUST002' TO WS-CUSTOMER-ID.
           MOVE 'PROD67890' TO WS-PRODUCT-ID.
           MOVE 50 TO WS-QUANTITY.
           MOVE 120.00 TO WS-UNIT-PRICE.
           MOVE '2024-02-05' TO WS-ORDER-DATE.
           MOVE 'PENDING' TO WS-STATUS.
           MOVE 3 TO WS-PRIORITY.
           MOVE 'Standard delivery' TO WS-NOTES.
           PERFORM SEND-KEYED-ORDER.
           
           MOVE 'ORD0003' TO WS-ORDER-KEY.
           MOVE 'ORD0003' TO WS-ORDER-ID.
           MOVE 'CUST003' TO WS-CUSTOMER-ID.
           MOVE 'PROD11111' TO WS-PRODUCT-ID.
           MOVE 200 TO WS-QUANTITY.
           MOVE 75.50 TO WS-UNIT-PRICE.
           MOVE '2024-02-05' TO WS-ORDER-DATE.
           MOVE 'PENDING' TO WS-STATUS.
           MOVE 2 TO WS-PRIORITY.
           MOVE 'Express delivery' TO WS-NOTES.
           PERFORM SEND-KEYED-ORDER.
           
       SEND-KEYED-ORDER.
           CALL 'QSNDDTAQ' USING
               WS-KDQ-NAME
               WS-KDQ-LIBRARY
               WS-KDQ-ENTRY-LENGTH
               WS-ORDER-ENTRY
               WS-KDQ-KEY-LENGTH
               WS-ORDER-KEY.
               
           DISPLAY 'Order added: ' WS-ORDER-ID 
                   ' Priority: ' WS-PRIORITY.
                   
       SEARCH-ORDERS.
           DISPLAY ' '.
           DISPLAY '=== Searching Orders ==='.
           
      *    搜尋特定訂單
           MOVE 'ORD0002' TO WS-SEARCH-KEY.
           SET SEARCH-EQUAL TO TRUE.
           PERFORM SEARCH-SPECIFIC-ORDER.
           
      *    搜尋大於特定鍵值的訂單
           MOVE 'ORD0001' TO WS-SEARCH-KEY.
           SET SEARCH-GREATER TO TRUE.
           PERFORM SEARCH-RANGE-ORDERS.
           
       SEARCH-SPECIFIC-ORDER.
           CALL 'QRCVDTAQ' USING
               WS-KDQ-NAME
               WS-KDQ-LIBRARY
               WS-KDQ-ENTRY-LENGTH
               WS-ORDER-ENTRY
               0                        *不等待
               WS-KDQ-KEY-LENGTH
               WS-SEARCH-KEY
               1                        *鍵值長度
               WS-SEARCH-ORDER
               WS-DQ-SENDER-INFO.
               
           IF WS-KDQ-ENTRY-LENGTH > 0
               DISPLAY 'Found order: ' WS-ORDER-ID
               DISPLAY 'Customer: ' WS-CUSTOMER-ID
               DISPLAY 'Status: ' WS-STATUS
           ELSE
               DISPLAY 'Order not found: ' WS-SEARCH-KEY
           END-IF.
           
       SEARCH-RANGE-ORDERS.
           DISPLAY 'Orders with key > ' WS-SEARCH-KEY ':'.
           
           PERFORM UNTIL WS-KDQ-ENTRY-LENGTH = 0
               CALL 'QRCVDTAQ' USING
                   WS-KDQ-NAME
                   WS-KDQ-LIBRARY
                   WS-KDQ-ENTRY-LENGTH
                   WS-ORDER-ENTRY
                   0
                   WS-KDQ-KEY-LENGTH
                   WS-SEARCH-KEY
                   1
                   WS-SEARCH-ORDER
                   WS-DQ-SENDER-INFO
                   
               IF WS-KDQ-ENTRY-LENGTH > 0
                   DISPLAY '  ' WS-ORDER-ID 
                           ' - Priority: ' WS-PRIORITY
               END-IF
           END-PERFORM.
           
       PROCESS-HIGH-PRIORITY-ORDERS.
           DISPLAY ' '.
           DISPLAY '=== Processing High Priority Orders ==='.
           
      *    處理優先級1和2的訂單
           PERFORM UNTIL WS-KDQ-ENTRY-LENGTH = 0
               MOVE SPACES TO WS-ORDER-ENTRY
               
               CALL 'QRCVDTAQ' USING
                   WS-KDQ-NAME
                   WS-KDQ-LIBRARY
                   WS-KDQ-ENTRY-LENGTH
                   WS-ORDER-ENTRY
                   0
                   WS-KDQ-KEY-LENGTH
                   ' '                  *任何鍵值
                   0                    *鍵長度0表示任何鍵
                   'EQ'
                   WS-DQ-SENDER-INFO
                   
               IF WS-KDQ-ENTRY-LENGTH > 0
                   IF WS-PRIORITY <= 2
                       PERFORM PROCESS-URGENT-ORDER
                   ELSE
                       PERFORM REQUEUE-NORMAL-ORDER
                   END-IF
               END-IF
           END-PERFORM.
           
       PROCESS-URGENT-ORDER.
           DISPLAY 'Processing urgent order: ' WS-ORDER-ID.
           MOVE 'PROCESSING' TO WS-STATUS.
           
      *    這裡可以加入實際的訂單處理邏輯
           DISPLAY '  Customer: ' WS-CUSTOMER-ID.
           DISPLAY '  Product: ' WS-PRODUCT-ID.
           DISPLAY '  Quantity: ' WS-QUANTITY.
           DISPLAY '  Notes: ' WS-NOTES.
           
       REQUEUE-NORMAL-ORDER.
      *    將一般優先級訂單放回佇列
           CALL 'QSNDDTAQ' USING
               WS-KDQ-NAME
               WS-KDQ-LIBRARY
               WS-KDQ-ENTRY-LENGTH
               WS-ORDER-ENTRY
               WS-KDQ-KEY-LENGTH
               WS-ORDER-ID.
```

### 2.2 使用者空間 (User Space)

#### 2.2.1 使用者空間基本概念

```cobol
      *    使用者空間是AS/400中用於大量資料共享的機制
      *    
      *    特色：
      *    - 可動態調整大小 (最大16MB)
      *    - 支援多程式並行存取
      *    - 提供指標式存取
      *    - 可設定自動擴展
      *    - 具備完整的權限控制
      
      *    使用場景：
      *    - 大型資料緩存
      *    - 程式間資料共享
      *    - 暫存運算結果
      *    - 系統資訊收集
```

#### 2.2.2 使用者空間操作

```cobol
      *****************************************************************
      * 程式名稱：USERSPACE                                          *
      * 程式功能：使用者空間示範程式                                 *
      * 作者：系統整合工程師                                         *
      * 日期：2024/02/05                                             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERSPACE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *    使用者空間參數
       01  WS-USER-SPACE-PARAMS.
           05  WS-US-NAME          PIC X(10) VALUE 'EMPDATA'.
           05  WS-US-LIBRARY       PIC X(10) VALUE 'QGPL'.
           05  WS-US-EXTENDED-ATTR PIC X(10) VALUE 'USRSPC'.
           05  WS-US-INITIAL-SIZE  PIC 9(9) COMP VALUE 65536.
           05  WS-US-INITIAL-VALUE PIC X VALUE X'00'.
           05  WS-US-PUBLIC-AUTH   PIC X(10) VALUE '*USE'.
           05  WS-US-DESCRIPTION   PIC X(50) VALUE 'Employee Data Cache'.
           
      *    指標操作參數
       01  WS-POINTER-PARAMS.
           05  WS-SPACE-POINTER    POINTER.
           05  WS-OFFSET           PIC 9(9) COMP VALUE 1.
           05  WS-DATA-LENGTH      PIC 9(9) COMP.
           
      *    員工資料結構
       01  WS-EMPLOYEE-CACHE.
           05  WS-CACHE-HEADER.
               10  WS-RECORD-COUNT     PIC 9(6) COMP.
               10  WS-LAST-UPDATE      PIC X(19).
               10  WS-CACHE-STATUS     PIC X(10).
               10  WS-RESERVED         PIC X(15).
           05  WS-EMPLOYEE-ENTRIES OCCURS 1000 TIMES 
                                   INDEXED BY WS-EMP-IDX.
               10  WS-EMP-ID           PIC X(6).
               10  WS-EMP-NAME         PIC X(30).
               10  WS-EMP-DEPT         PIC X(4).
               10  WS-EMP-SALARY       PIC 9(8)V99 COMP-3.
               10  WS-EMP-STATUS       PIC X.
               10  WS-EMP-LAST-ACCESS  PIC X(19).
               
      *    工作變數
       01  WS-WORK-FIELDS.
           05  WS-CURRENT-TIME     PIC X(19).
           05  WS-CACHE-SIZE       PIC 9(9) COMP.
           
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-USER-SPACE
           PERFORM LOAD-EMPLOYEE-CACHE
           PERFORM ACCESS-CACHED-DATA
           PERFORM UPDATE-CACHE-STATUS
           STOP RUN.
           
       INITIALIZE-USER-SPACE.
           DISPLAY 'Initializing User Space...'.
           
      *    建立使用者空間
           CALL 'QUSCRTUS' USING
               WS-US-NAME
               WS-US-LIBRARY
               WS-US-EXTENDED-ATTR
               WS-US-INITIAL-SIZE
               WS-US-INITIAL-VALUE
               WS-US-PUBLIC-AUTH
               WS-US-DESCRIPTION
               '*YES'               *可替換
               WS-ERROR-CODE.
               
           IF WS-ERROR-AVAILABLE > 0
               DISPLAY 'Error creating user space: ' WS-ERROR-MSGID
               STOP RUN
           END-IF.
           
      *    取得使用者空間指標
           CALL 'QUSPTRUS' USING
               WS-US-NAME
               WS-US-LIBRARY
               WS-SPACE-POINTER
               WS-ERROR-CODE.
               
           IF WS-ERROR-AVAILABLE > 0
               DISPLAY 'Error getting user space pointer: ' WS-ERROR-MSGID
               STOP RUN
           END-IF.
           
           DISPLAY 'User Space initialized successfully'.
           
       LOAD-EMPLOYEE-CACHE.
           DISPLAY 'Loading Employee Data into Cache...'.
           
      *    初始化快取標頭
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME.
           MOVE 0 TO WS-RECORD-COUNT.
           MOVE WS-CURRENT-TIME TO WS-LAST-UPDATE.
           MOVE 'LOADING' TO WS-CACHE-STATUS.
           
      *    載入員工資料 (這裡用範例資料)
           PERFORM LOAD-SAMPLE-EMPLOYEES.
           
      *    寫入快取到使用者空間
           MOVE LENGTH OF WS-EMPLOYEE-CACHE TO WS-DATA-LENGTH.
           
           CALL 'QUSLSPC' USING
               WS-US-NAME
               WS-US-LIBRARY
               WS-OFFSET
               WS-DATA-LENGTH
               WS-EMPLOYEE-CACHE
               WS-ERROR-CODE.
               
           MOVE 'READY' TO WS-CACHE-STATUS.
           DISPLAY 'Employee cache loaded: ' WS-RECORD-COUNT ' records'.
           
       LOAD-SAMPLE-EMPLOYEES.
      *    載入範例員工資料
           SET WS-EMP-IDX TO 1.
           
           MOVE 'E00001' TO WS-EMP-ID(WS-EMP-IDX).
           MOVE '張志明' TO WS-EMP-NAME(WS-EMP-IDX).
           MOVE 'IT01' TO WS-EMP-DEPT(WS-EMP-IDX).
           MOVE 75000.00 TO WS-EMP-SALARY(WS-EMP-IDX).
           MOVE 'A' TO WS-EMP-STATUS(WS-EMP-IDX).
           MOVE WS-CURRENT-TIME TO WS-EMP-LAST-ACCESS(WS-EMP-IDX).
           ADD 1 TO WS-RECORD-COUNT.
           
           SET WS-EMP-IDX UP BY 1.
           MOVE 'E00002' TO WS-EMP-ID(WS-EMP-IDX).
           MOVE '李美玲' TO WS-EMP-NAME(WS-EMP-IDX).
           MOVE 'HR01' TO WS-EMP-DEPT(WS-EMP-IDX).
           MOVE 68000.00 TO WS-EMP-SALARY(WS-EMP-IDX).
           MOVE 'A' TO WS-EMP-STATUS(WS-EMP-IDX).
           MOVE WS-CURRENT-TIME TO WS-EMP-LAST-ACCESS(WS-EMP-IDX).
           ADD 1 TO WS-RECORD-COUNT.
           
           SET WS-EMP-IDX UP BY 1.
           MOVE 'E00003' TO WS-EMP-ID(WS-EMP-IDX).
           MOVE '王建國' TO WS-EMP-NAME(WS-EMP-IDX).
           MOVE 'FN01' TO WS-EMP-DEPT(WS-EMP-IDX).
           MOVE 72000.00 TO WS-EMP-SALARY(WS-EMP-IDX).
           MOVE 'A' TO WS-EMP-STATUS(WS-EMP-IDX).
           MOVE WS-CURRENT-TIME TO WS-EMP-LAST-ACCESS(WS-EMP-IDX).
           ADD 1 TO WS-RECORD-COUNT.

       ACCESS-CACHED-DATA.
           DISPLAY 'Accessing Employee Data from User Space...'.
           MOVE 1 TO WS-EMP-IDX.
           PERFORM UNTIL WS-EMP-IDX > WS-RECORD-COUNT
               DISPLAY '員工編號: ' WS-EMP-ID(WS-EMP-IDX)
               DISPLAY '姓名: ' WS-EMP-NAME(WS-EMP-IDX)
               DISPLAY '部門: ' WS-EMP-DEPT(WS-EMP-IDX)
               DISPLAY '薪資: ' WS-EMP-SALARY(WS-EMP-IDX)
               DISPLAY '狀態: ' WS-EMP-STATUS(WS-EMP-IDX)
               DISPLAY '最近存取: ' WS-EMP-LAST-ACCESS(WS-EMP-IDX)
               DISPLAY '-----------------------------'
               ADD 1 TO WS-EMP-IDX
           END-PERFORM.

       UPDATE-CACHE-STATUS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME.
           MOVE WS-CURRENT-TIME TO WS-LAST-UPDATE.
           MOVE 'UPDATED' TO WS-CACHE-STATUS.
           DISPLAY 'User Space cache status updated.'

---

## 📝 本週小結

- 本週學習了企業級系統架構、三層式與微服務架構在AS/400的應用。
- 熟悉了AS/400的程式間通訊（IPC）技術，包括資料佇列、鍵值資料佇列與使用者空間。
- 掌握了資料佇列的建立、訊息傳遞、搜尋與高優先級處理技巧。
- 學會了使用者空間的建立、資料快取與多程式共享應用。
- 透過實例練習，能夠設計跨系統整合與高效資料交換機制。

---

## 📌 課後練習

1. 請設計一個COBOL程式，模擬兩個程式透過資料佇列進行訊息交換。
2. 修改鍵值資料佇列範例，實作訂單狀態查詢與更新功能。
3. 嘗試設計一個使用者空間快取，支援動態新增、查詢與刪除員工資料。

---