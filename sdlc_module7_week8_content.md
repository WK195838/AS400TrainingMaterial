# 第七模組：部署與維護 (第8週)

**學習時間**：第8週 (6-8小時)  
**先修要求**：完成第六模組 - 軟體測試  
**本週重點**：軟體部署策略與維護管理的完整體系

---

## 📋 學習目標

完成本週學習後，您將能夠：

✅ **掌握現代軟體部署策略與最佳實務**  
✅ **建立有效的系統監控與效能調校機制**  
✅ **設計完整的維護管理流程**  
✅ **實施災難恢復與備份策略**  
✅ **建立使用者支援與文件管理體系**  
✅ **規劃軟體生命週期的長期維護**

---

## 🎯 本週學習路徑

```
部署策略設計 → 系統監控建立 → 維護流程規劃 → 災難恢復準備 → 使用者支援 → 生命週期管理
      ↓             ↓            ↓           ↓            ↓           ↓
   部署自動化      效能監控      版本維護    備份恢復      支援體系     永續經營
```

---

## 🚀 第一部分：部署策略與實務

### 1.1 部署基礎概念

#### 📝 部署的定義與重要性

**軟體部署 (Software Deployment)** 是將軟體從開發環境遷移到生產環境，使其能夠為使用者提供服務的過程。

**部署的關鍵要素**：
- **可靠性**：部署過程穩定且可重複
- **可預測性**：部署結果可預期
- **可回復性**：出問題時能快速回復
- **可觀測性**：部署過程透明可監控

#### 📊 部署失敗的成本分析

**業界統計顯示**：
- 每次部署失敗平均造成 **$300,000** 損失
- 部署問題導致的停機時間平均為 **4.2小時**
- 手動部署的錯誤率是自動化部署的 **10倍**
- 有效的部署策略可以減少 **90%** 的部署相關問題

### 1.2 部署環境架構

#### 🏗️ 多環境部署策略

**標準環境架構**：
```
開發環境 (Development)
    ↓
測試環境 (Testing)
    ↓
預產環境 (Staging)
    ↓
生產環境 (Production)
```

**各環境特性**：

| 環境 | 目的 | 數據 | 流量 | 監控 | 自動化程度 |
|------|------|------|------|------|------------|
| **開發** | 功能開發 | 模擬數據 | 開發者 | 基本 | 高 |
| **測試** | 功能測試 | 測試數據 | QA團隊 | 中等 | 高 |
| **預產** | 發布驗證 | 生產複本 | 內部使用者 | 完整 | 中等 |
| **生產** | 正式服務 | 真實數據 | 所有使用者 | 全面 | 低 |

#### 🔧 環境配置管理

**Infrastructure as Code (IaC)**：
```yaml
# docker-compose.yml - 開發環境
version: '3.8'
services:
  app:
    build: .
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=development
      - DB_HOST=db
      - REDIS_URL=redis://redis:6379
    volumes:
      - .:/app
      - /app/node_modules
    depends_on:
      - db
      - redis

  db:
    image: postgres:14
    environment:
      POSTGRES_DB: myapp_dev
      POSTGRES_USER: developer
      POSTGRES_PASSWORD: devpass
    volumes:
      - dev_db_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  dev_db_data:
```

**Kubernetes生產環境配置**：
```yaml
# k8s/production/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp-production
  namespace: production
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
      env: production
  template:
    metadata:
      labels:
        app: myapp
        env: production
    spec:
      containers:
      - name: app
        image: myapp:v1.2.3
        ports:
        - containerPort: 3000
        env:
        - name: NODE_ENV
          value: "production"
        - name: DB_HOST
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: host
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: password
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5

---
apiVersion: v1
kind: Service
metadata:
  name: myapp-service
  namespace: production
spec:
  selector:
    app: myapp
    env: production
  ports:
  - port: 80
    targetPort: 3000
  type: LoadBalancer
```

### 1.3 部署策略

#### 🔄 藍綠部署 (Blue-Green Deployment)

**概念圖**：
```
Load Balancer
    │
    ├─ 藍環境 (v1.0) ← 目前生產流量
    │
    └─ 綠環境 (v1.1) ← 新版本待命
    
切換後：
Load Balancer
    │
    ├─ 藍環境 (v1.0) ← 待機環境
    │
    └─ 綠環境 (v1.1) ← 新的生產流量
```

**優點**：
- **零停機時間**：瞬間切換
- **快速回復**：有問題立即切回
- **完整測試**：新版本完全驗證後才切換

**實作範例**：
```bash
#!/bin/bash
# blue-green-deploy.sh

CURRENT_ENV=$(kubectl get service myapp-service -o jsonpath='{.spec.selector.version}')
NEW_ENV=$([ "$CURRENT_ENV" == "blue" ] && echo "green" || echo "blue")

echo "目前環境: $CURRENT_ENV"
echo "部署到: $NEW_ENV"

# 部署新版本到待機環境
kubectl set image deployment/myapp-$NEW_ENV app=myapp:$1 --namespace=production

# 等待部署完成
kubectl rollout status deployment/myapp-$NEW_ENV --namespace=production

# 執行健康檢查
if curl -f http://myapp-$NEW_ENV.production.svc.cluster.local/health; then
    echo "健康檢查通過，切換流量"
    
    # 切換Service指向新環境
    kubectl patch service myapp-service -p '{"spec":{"selector":{"version":"'$NEW_ENV'"}}}'
    
    echo "部署完成！流量已切換到 $NEW_ENV 環境"
else
    echo "健康檢查失敗，取消部署"
    exit 1
fi
```

#### 🎯 金絲雀部署 (Canary Deployment)

**漸進式流量分配**：
```
階段1: 95%流量 → 舊版本, 5%流量 → 新版本
階段2: 80%流量 → 舊版本, 20%流量 → 新版本
階段3: 50%流量 → 舊版本, 50%流量 → 新版本
階段4: 0%流量 → 舊版本, 100%流量 → 新版本
```

**Istio金絲雀部署配置**：
```yaml
# istio/canary-deployment.yaml
apiVersion: networking.istio.io/v1alpha3
kind: VirtualService
metadata:
  name: myapp-canary
spec:
  http:
  - match:
    - headers:
        canary:
          exact: "true"
    route:
    - destination:
        host: myapp-service
        subset: v2
      weight: 100
  - route:
    - destination:
        host: myapp-service
        subset: v1
      weight: 90
    - destination:
        host: myapp-service
        subset: v2
      weight: 10

---
apiVersion: networking.istio.io/v1alpha3
kind: DestinationRule
metadata:
  name: myapp-destination
spec:
  host: myapp-service
  subsets:
  - name: v1
    labels:
      version: v1
  - name: v2
    labels:
      version: v2
```

#### 🌊 滾動更新 (Rolling Update)

**Kubernetes滾動更新策略**：
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp-rolling
spec:
  replicas: 6
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 1      # 最多1個Pod不可用
      maxSurge: 2           # 最多額外2個Pod
  template:
    spec:
      containers:
      - name: app
        image: myapp:v1.2.3
        readinessProbe:
          httpGet:
            path: /ready
            port: 3000
          periodSeconds: 5
          successThreshold: 2
```

**滾動更新腳本**：
```bash
#!/bin/bash
# rolling-update.sh

APP_NAME="myapp"
NEW_IMAGE="myapp:$1"
NAMESPACE="production"

echo "開始滾動更新到 $NEW_IMAGE"

# 更新鏡像
kubectl set image deployment/$APP_NAME app=$NEW_IMAGE -n $NAMESPACE

# 監控更新過程
kubectl rollout status deployment/$APP_NAME -n $NAMESPACE --timeout=600s

if [ $? -eq 0 ]; then
    echo "滾動更新成功完成"
    
    # 驗證更新
    kubectl get pods -l app=$APP_NAME -n $NAMESPACE
    
    # 檢查健康狀態
    for i in {1..5}; do
        if curl -f http://$APP_NAME.$NAMESPACE.svc.cluster.local/health; then
            echo "健康檢查通過"
            break
        else
            echo "等待服務就緒... ($i/5)"
            sleep 10
        fi
    done
else
    echo "滾動更新失敗，執行回復"
    kubectl rollout undo deployment/$APP_NAME -n $NAMESPACE
    exit 1
fi
```

### 1.4 自動化部署流程

#### 🤖 CI/CD Pipeline設計

**完整部署流程**：
```yaml
# .github/workflows/deploy.yml
name: Production Deployment

on:
  push:
    tags:
      - 'v*'

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  build:
    runs-on: ubuntu-latest
    outputs:
      image: ${{ steps.image.outputs.image }}
    
    steps:
    - name: Checkout
      uses: actions/checkout@v3
    
    - name: 設定Docker Buildx
      uses: docker/setup-buildx-action@v2
    
    - name: 登入Container Registry
      uses: docker/login-action@v2
      with:
        registry: ${{ env.REGISTRY }}
        username: ${{ github.actor }}
        password: ${{ secrets.GITHUB_TOKEN }}
    
    - name: 提取metadata
      id: meta
      uses: docker/metadata-action@v4
      with:
        images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
        tags: |
          type=ref,event=tag
          type=sha
    
    - name: 建置並推送Docker映像
      uses: docker/build-push-action@v4
      with:
        context: .
        push: true
        tags: ${{ steps.meta.outputs.tags }}
        labels: ${{ steps.meta.outputs.labels }}
        cache-from: type=gha
        cache-to: type=gha,mode=max
    
    - name: 輸出映像名稱
      id: image
      run: echo "image=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:${{ github.ref_name }}" >> $GITHUB_OUTPUT

  security-scan:
    needs: build
    runs-on: ubuntu-latest
    
    steps:
    - name: 安全掃描
      uses: aquasecurity/trivy-action@master
      with:
        image-ref: ${{ needs.build.outputs.image }}
        format: 'sarif'
        output: 'trivy-results.sarif'
    
    - name: 上傳掃描結果
      uses: github/codeql-action/upload-sarif@v2
      with:
        sarif_file: 'trivy-results.sarif'

  deploy-staging:
    needs: [build, security-scan]
    runs-on: ubuntu-latest
    environment: staging
    
    steps:
    - name: 部署到Staging
      run: |
        echo "部署 ${{ needs.build.outputs.image }} 到staging環境"
        # 實際部署命令
        kubectl set image deployment/myapp-staging app=${{ needs.build.outputs.image }}
        kubectl rollout status deployment/myapp-staging
    
    - name: 執行煙霧測試
      run: |
        # 執行基本功能測試
        npm run test:smoke -- --env=staging

  deploy-production:
    needs: [build, deploy-staging]
    runs-on: ubuntu-latest
    environment: production
    
    steps:
    - name: 創建部署issue
      uses: actions/github-script@v6
      with:
        script: |
          github.rest.issues.create({
            owner: context.repo.owner,
            repo: context.repo.repo,
            title: `生產部署: ${{ github.ref_name }}`,
            body: `
              ## 部署資訊
              - **版本**: ${{ github.ref_name }}
              - **映像**: ${{ needs.build.outputs.image }}
              - **觸發者**: ${{ github.actor }}
              
              ## 檢查清單
              - [ ] 確認staging測試通過
              - [ ] 確認資料庫遷移計畫
              - [ ] 確認回復計畫
              - [ ] 通知相關團隊
            `
          })
    
    - name: 藍綠部署到生產環境
      run: |
        echo "開始藍綠部署到生產環境"
        ./scripts/blue-green-deploy.sh ${{ needs.build.outputs.image }}
    
    - name: 執行生產環境測試
      run: |
        npm run test:production
    
    - name: 發送部署通知
      uses: 8398a7/action-slack@v3
      with:
        status: success
        text: |
          🚀 生產部署成功！
          版本: ${{ github.ref_name }}
          部署者: ${{ github.actor }}
      env:
        SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK }}
```

---

## 📊 第二部分：系統監控與效能管理

### 2.1 監控策略設計

#### 🎯 監控的四個黃金指標

**SRE黃金指標**：
1. **延遲 (Latency)**：請求回應時間
2. **流量 (Traffic)**：系統處理的請求量
3. **錯誤率 (Errors)**：失敗請求的比例
4. **飽和度 (Saturation)**：系統資源使用程度

#### 📈 監控層級架構

```
業務監控 (Business Metrics)
    ↓
應用監控 (Application Metrics)  
    ↓
基礎設施監控 (Infrastructure Metrics)
    ↓
網路監控 (Network Metrics)
```

**各層級監控重點**：

| 層級 | 監控內容 | 工具 | 告警閾值 |
|------|----------|------|----------|
| **業務** | 註冊數、交易額、轉換率 | Google Analytics, Mixpanel | 業務KPI |
| **應用** | 回應時間、錯誤率、吞吐量 | Prometheus, New Relic | 99%可用性 |
| **基礎設施** | CPU、記憶體、磁碟、網路 | Grafana, DataDog | 80%使用率 |
| **網路** | 頻寬、延遲、封包遺失 | Nagios, Zabbix | 網路品質 |

### 2.2 Prometheus監控實作

#### 🔧 Prometheus配置

**prometheus.yml配置**：
```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

rule_files:
  - "alert_rules.yml"

alerting:
  alertmanagers:
    - static_configs:
        - targets:
          - alertmanager:9093

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  - job_name: 'myapp'
    static_configs:
      - targets: ['myapp:3000']
    metrics_path: '/metrics'
    scrape_interval: 5s

  - job_name: 'node'
    static_configs:
      - targets: ['node-exporter:9100']

  - job_name: 'postgres'
    static_configs:
      - targets: ['postgres-exporter:9187']
```

#### 📊 應用程式指標實作

**Node.js應用程式指標**：
```javascript
// metrics.js
const prometheus = require('prom-client');

// 建立指標收集器
const collectDefaultMetrics = prometheus.collectDefaultMetrics;
collectDefaultMetrics({ timeout: 5000 });

// 自定義業務指標
const httpRequestsTotal = new prometheus.Counter({
  name: 'http_requests_total',
  help: 'Total number of HTTP requests',
  labelNames: ['method', 'route', 'status_code']
});

const httpRequestDuration = new prometheus.Histogram({
  name: 'http_request_duration_seconds',
  help: 'Duration of HTTP requests in seconds',
  labelNames: ['method', 'route', 'status_code'],
  buckets: [0.1, 0.3, 0.5, 0.7, 1, 3, 5, 7, 10]
});

const activeUsers = new prometheus.Gauge({
  name: 'active_users_total',
  help: 'Total number of active users'
});

const databaseConnections = new prometheus.Gauge({
  name: 'database_connections_active',
  help: 'Number of active database connections'
});

// 中間件函數
function metricsMiddleware(req, res, next) {
  const start = Date.now();
  
  res.on('finish', () => {
    const duration = (Date.now() - start) / 1000;
    const route = req.route ? req.route.path : req.path;
    
    httpRequestsTotal
      .labels(req.method, route, res.statusCode)
      .inc();
    
    httpRequestDuration
      .labels(req.method, route, res.statusCode)
      .observe(duration);
  });
  
  next();
}

// 業務指標更新函數
function updateBusinessMetrics() {
  setInterval(async () => {
    try {
      // 更新活躍使用者數
      const activeUserCount = await getActiveUserCount();
      activeUsers.set(activeUserCount);
      
      // 更新資料庫連接數
      const dbConnections = await getDatabaseConnectionCount();
      databaseConnections.set(dbConnections);
      
    } catch (error) {
      console.error('更新業務指標失敗:', error);
    }
  }, 30000); // 每30秒更新一次
}

module.exports = {
  prometheus,
  metricsMiddleware,
  updateBusinessMetrics,
  register: prometheus.register
};

// app.js
const express = require('express');
const { metricsMiddleware, register, updateBusinessMetrics } = require('./metrics');

const app = express();

// 使用指標中間件
app.use(metricsMiddleware);

// 指標端點
app.get('/metrics', (req, res) => {
  res.set('Content-Type', register.contentType);
  res.end(register.metrics());
});

// 健康檢查端點
app.get('/health', (req, res) => {
  res.json({ status: 'healthy', timestamp: new Date().toISOString() });
});

// 啟動業務指標更新
updateBusinessMetrics();

app.listen(3000, () => {
  console.log('應用程式運行在 http://localhost:3000');
});
```

### 2.3 告警系統設計

#### 🚨 告警規則配置

**alert_rules.yml**：
```yaml
groups:
- name: application.rules
  rules:
  - alert: HighErrorRate
    expr: rate(http_requests_total{status_code=~"5.."}[5m]) > 0.1
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "高錯誤率偵測"
      description: "應用程式錯誤率超過10%，持續5分鐘"

  - alert: HighResponseTime
    expr: histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m])) > 1
    for: 3m
    labels:
      severity: warning
    annotations:
      summary: "回應時間過長"
      description: "95%回應時間超過1秒，持續3分鐘"

  - alert: DatabaseConnectionHigh
    expr: database_connections_active > 80
    for: 2m
    labels:
      severity: warning
    annotations:
      summary: "資料庫連接數過高"
      description: "資料庫連接數超過80，目前: {{ $value }}"

- name: infrastructure.rules
  rules:
  - alert: HighCPUUsage
    expr: 100 - (avg(rate(node_cpu_seconds_total{mode="idle"}[5m])) * 100) > 85
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "CPU使用率過高"
      description: "CPU使用率超過85%，持續5分鐘"

  - alert: HighMemoryUsage
    expr: (1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100 > 90
    for: 3m
    labels:
      severity: critical
    annotations:
      summary: "記憶體使用率過高"
      description: "記憶體使用率超過90%，持續3分鐘"

  - alert: DiskSpaceLow
    expr: (1 - (node_filesystem_avail_bytes / node_filesystem_size_bytes)) * 100 > 85
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "磁碟空間不足"
      description: "磁碟使用率超過85%，剩余空間: {{ $value }}%"
```

#### 📱 Alertmanager通知配置

**alertmanager.yml**：
```yaml
global:
  smtp_smarthost: 'smtp.gmail.com:587'
  smtp_from: 'alerts@mycompany.com'

route:
  group_by: ['alertname']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 1h
  receiver: 'web.hook'
  routes:
  - match:
      severity: critical
    receiver: 'critical-alerts'
    group_wait: 0s
    repeat_interval: 5m
  - match:
      severity: warning
    receiver: 'warning-alerts'

receivers:
- name: 'web.hook'
  webhook_configs:
  - url: 'http://webhook-server:5000/webhook'

- name: 'critical-alerts'
  slack_configs:
  - api_url: 'YOUR_SLACK_WEBHOOK_URL'
    channel: '#critical-alerts'
    title: '🚨 嚴重告警'
    text: |
      {{ range .Alerts }}
      告警: {{ .Annotations.summary }}
      描述: {{ .Annotations.description }}
      時間: {{ .StartsAt }}
      {{ end }}
  email_configs:
  - to: 'oncall@mycompany.com'
    subject: '【緊急】生產環境告警'
    body: |
      {{ range .Alerts }}
      告警名稱: {{ .Annotations.summary }}
      詳細描述: {{ .Annotations.description }}
      告警時間: {{ .StartsAt }}
      {{ end }}

- name: 'warning-alerts'
  slack_configs:
  - api_url: 'YOUR_SLACK_WEBHOOK_URL'
    channel: '#monitoring'
    title: '⚠️ 警告告警'
    text: |
      {{ range .Alerts }}
      告警: {{ .Annotations.summary }}
      描述: {{ .Annotations.description }}
      {{ end }}
```

### 2.4 效能調校

#### ⚡ 應用程式效能優化

**Node.js效能優化技巧**：
```javascript
// performance-optimizations.js

// 1. 連接池優化
const { Pool } = require('pg');
const pool = new Pool({
  host: 'localhost',
  user: 'dbuser',
  password: 'secretpassword',
  database: 'mydb',
  max: 20,                // 最大連接數
  idleTimeoutMillis: 30000, // 閒置超時
  connectionTimeoutMillis: 2000, // 連接超時
});

// 2. 快取策略
const Redis = require('redis');
const redis = Redis.createClient({
  host: 'redis-server',
  port: 6379,
  retry_strategy: (options) => {
    if (options.error && options.error.code === 'ECONNREFUSED') {
      return new Error('Redis服務器拒絕連接');
    }
    if (options.total_retry_time > 1000 * 60 * 60) {
      return new Error('重試時間超過1小時');
    }
    if (options.attempt > 10) {
      return undefined;
    }
    return Math.min(options.attempt * 100, 3000);
  }
});

// 快取中間件
function cacheMiddleware(duration = 300) {
  return async (req, res, next) => {
    const key = req.originalUrl;
    
    try {
      const cached = await redis.get(key);
      if (cached) {
        return res.json(JSON.parse(cached));
      }
      
      // 覆寫res.json以快取結果
      const originalJson = res.json;
      res.json = function(data) {
        redis.setex(key, duration, JSON.stringify(data));
        return originalJson.call(this, data);
      };
      
      next();
    } catch (error) {
      console.error('快取錯誤:', error);
      next();
    }
  };
}

// 3. 資料庫查詢優化
class UserService {
  // 批次查詢優化
  async getUsersWithPosts(userIds) {
    // ❌ N+1查詢問題
    // const users = await User.findAll({ where: { id: userIds } });
    // for (const user of users) {
    //   user.posts = await Post.findAll({ where: { userId: user.id } });
    // }
    
    // ✅ 優化的查詢
    const users = await User.findAll({
      where: { id: userIds },
      include: [{
        model: Post,
        required: false
      }]
    });
    
    return users;
  }
  
  // 分頁查詢優化
  async getUsersPaginated(page, limit) {
    const offset = (page - 1) * limit;
    
    // 使用子查詢優化大表分頁
    const { count, rows } = await User.findAndCountAll({
      attributes: ['id', 'name', 'email'],
      order: [['created_at', 'DESC']],
      limit,
      offset,
      distinct: true
    });
    
    return {
      users: rows,
      totalPages: Math.ceil(count / limit),
      currentPage: page,
      total: count
    };
  }
}

// 4. 壓縮和靜態資源優化
const compression = require('compression');
const express = require('express');
const app = express();

// 啟用GZIP壓縮
app.use(compression({
  filter: (req, res) => {
    if (req.headers['x-no-compression']) {
      return false;
    }
    return compression.filter(req, res);
  },
  level: 6,
  threshold: 1024
}));

// 靜態資源快取
app.use('/static', express.static('public', {
  maxAge: '1d',
  etag: true,
  lastModified: true
}));

// 5. 請求限制和防濫用
const rateLimit = require('express-rate-limit');

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15分鐘
  max: 100, // 限制每個IP最多100個請求
  message: {
    error: '請求過於頻繁，請稍後再試',
    retryAfter: 15 * 60
  },
  standardHeaders: true,
  legacyHeaders: false
});

app.use('/api', limiter);

module.exports = {
  pool,
  redis,
  cacheMiddleware,
  UserService
};
```

#### 🗄️ 資料庫效能優化

**PostgreSQL優化配置**：
```sql
-- postgresql.conf優化設定

-- 記憶體設定
shared_buffers = 256MB              -- 共享緩衝區
effective_cache_size = 1GB          -- 有效快取大小
work_mem = 4MB                      -- 工作記憶體
maintenance_work_mem = 64MB         -- 維護工作記憶體

-- 連接設定
max_connections = 100               -- 最大連接數
shared_preload_libraries = 'pg_stat_statements'

-- 查詢優化
random_page_cost = 1.1              -- 隨機頁面成本
effective_io_concurrency = 200      -- 有效IO並發

-- 日誌設定
log_statement = 'all'               -- 記錄所有語句
log_min_duration_statement = 1000   -- 記錄執行超過1秒的查詢
log_line_prefix = '%t [%p]: [%l-1] user=%u,db=%d,app=%a,client=%h '
```

**查詢優化範例**：
```sql
-- 1. 索引優化
-- 查詢執行計畫分析
EXPLAIN (ANALYZE, BUFFERS) 
SELECT u.name, p.title 
FROM users u 
JOIN posts p ON u.id = p.user_id 
WHERE u.created_at > '2024-01-01';

-- 建立複合索引
CREATE INDEX CONCURRENTLY idx_users_created_at_id 
ON users(created_at, id);

CREATE INDEX CONCURRENTLY idx_posts_user_id_title 
ON posts(user_id, title);

-- 2. 分區表優化
-- 按時間分區的日誌表
CREATE TABLE access_logs (
    id BIGSERIAL,
    user_id INTEGER,
    endpoint VARCHAR(255),
    response_time INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
) PARTITION BY RANGE (created_at);

-- 建立分區
CREATE TABLE access_logs_2024_q1 
PARTITION OF access_logs 
FOR VALUES FROM ('2024-01-01') TO ('2024-04-01');

CREATE TABLE access_logs_2024_q2 
PARTITION OF access_logs 
FOR VALUES FROM ('2024-04-01') TO ('2024-07-01');

-- 3. 查詢優化技巧
-- 使用EXISTS代替IN (子查詢)
-- ❌ 效能較差
SELECT * FROM users 
WHERE id IN (SELECT user_id FROM posts WHERE status = 'published');

-- ✅ 效能較好
SELECT * FROM users u
WHERE EXISTS (
    SELECT 1 FROM posts p 
    WHERE p.user_id = u.id AND p.status = 'published'
);

-- 4. 批次操作優化
-- 批次插入
INSERT INTO users (name, email, created_at)
SELECT 
    'User ' || generate_series(1, 1000),
    'user' || generate_series(1, 1000) || '@example.com',
    CURRENT_TIMESTAMP
ON CONFLICT (email) DO NOTHING;

-- 批次更新
UPDATE posts 
SET view_count = view_count + views.increment
FROM (VALUES 
    (1, 10),
    (2, 15),
    (3, 5)
) AS views(post_id, increment)
WHERE posts.id = views.post_id;
```

---

## 🔧 第三部分：維護管理

### 3.1 維護策略規劃

#### 📋 軟體維護類型

**IEEE標準維護分類**：

1. **修正性維護 (Corrective Maintenance)**
   - **目的**：修復已發現的錯誤和缺陷
   - **比例**：約20%的維護工作
   - **優先級**：最高

2. **適應性維護 (Adaptive Maintenance)**
   - **目的**：適應環境變化（OS、硬體、法規）
   - **比例**：約25%的維護工作
   - **特點**：預防性

3. **完善性維護 (Perfective Maintenance)**
   - **目的**：改善功能和效能
   - **比例**：約50%的維護工作
   - **驅動**：使用者需求

4. **預防性維護 (Preventive Maintenance)**
   - **目的**：預防未來問題
   - **比例**：約5%的維護工作
   - **價值**：長期穩定

#### 📊 維護成本分析

**維護成本分佈**：
```
軟體生命週期總成本
├── 開發成本 (30%)
│   ├── 需求分析 (5%)
│   ├── 設計 (8%)
│   ├── 編碼 (12%)
│   └── 測試 (5%)
└── 維護成本 (70%)
    ├── 修正性維護 (14%)
    ├── 適應性維護 (18%)
    ├── 完善性維護 (35%)
    └── 預防性維護 (3%)
```

### 3.2 版本管理策略

#### 🏷️ 語意化版本控制實務

**版本號格式：MAJOR.MINOR.PATCH**

```javascript
// version-manager.js
class VersionManager {
  constructor(currentVersion = '1.0.0') {
    this.version = this.parseVersion(currentVersion);
  }
  
  parseVersion(versionString) {
    const [major, minor, patch] = versionString.split('.').map(Number);
    return { major, minor, patch };
  }
  
  // 主版本號：不相容的API變更
  incrementMajor() {
    this.version.major += 1;
    this.version.minor = 0;
    this.version.patch = 0;
    return this.toString();
  }
  
  // 次版本號：向後相容的新功能
  incrementMinor() {
    this.version.minor += 1;
    this.version.patch = 0;
    return this.toString();
  }
  
  // 修訂版本號：向後相容的錯誤修復
  incrementPatch() {
    this.version.patch += 1;
    return this.toString();
  }
  
  toString() {
    return `${this.version.major}.${this.version.minor}.${this.version.patch}`;
  }
  
  // 預發布版本
  createPrerelease(type = 'alpha', number = 1) {
    return `${this.toString()}-${type}.${number}`;
  }
}

// 使用範例
const vm = new VersionManager('2.1.3');
console.log(vm.incrementPatch()); // 2.1.4
console.log(vm.incrementMinor()); // 2.2.0
console.log(vm.createPrerelease('beta', 2)); // 2.2.0-beta.2
```

#### 🔄 發布管理流程

**自動化版本發布腳本**：
```bash
#!/bin/bash
# release.sh

set -e

# 參數驗證
if [ $# -eq 0 ]; then
    echo "使用方法: $0 <patch|minor|major>"
    exit 1
fi

RELEASE_TYPE=$1
CURRENT_BRANCH=$(git branch --show-current)

# 確保在main分支
if [ "$CURRENT_BRANCH" != "main" ]; then
    echo "錯誤：必須在main分支上執行發布"
    exit 1
fi

# 確保工作目錄乾淨
if [ -n "$(git status --porcelain)" ]; then
    echo "錯誤：工作目錄有未提交的變更"
    exit 1
fi

# 拉取最新代碼
git pull origin main

# 執行測試
echo "執行測試套件..."
npm test
if [ $? -ne 0 ]; then
    echo "錯誤：測試失敗"
    exit 1
fi

# 更新版本號
echo "更新版本號..."
NEW_VERSION=$(npm version $RELEASE_TYPE --no-git-tag-version)
echo "新版本: $NEW_VERSION"

# 更新CHANGELOG
echo "更新CHANGELOG..."
npx auto-changelog

# 提交版本變更
git add package.json package-lock.json CHANGELOG.md
git commit -m "chore: release $NEW_VERSION"

# 建立標籤
git tag -a $NEW_VERSION -m "Release $NEW_VERSION"

# 推送到遠程倉庫
git push origin main
git push origin $NEW_VERSION

echo "發布 $NEW_VERSION 完成！"

# 觸發CI/CD部署
echo "觸發自動部署..."
gh workflow run deploy.yml --ref $NEW_VERSION
```

### 3.3 變更管理

#### 📝 變更控制流程

**RFC (Request for Comments) 流程**：
```markdown
# RFC-001: 資料庫分片架構改造

## 摘要
為了支撐快速增長的使用者量，提議將現有單體資料庫改造為分片架構。

## 動機
- 目前資料庫QPS已達到70%上限
- 資料量預計在6個月內增長3倍
- 單點故障風險過高

## 詳細設計
### 分片策略
- 按用戶ID進行水平分片
- 使用一致性哈希算法
- 初期建立4個分片

### 遷移計畫
1. 建立新的分片資料庫
2. 實作分片路由層
3. 雙寫模式驗證
4. 逐步遷移歷史資料
5. 切換讀寫流量

## 風險與緩解
| 風險 | 機率 | 影響 | 緩解策略 |
|------|------|------|----------|
| 資料不一致 | 中 | 高 | 雙寫驗證 + 資料對比 |
| 效能下降 | 低 | 中 | 充分測試 + 回復計畫 |
| 停機時間 | 中 | 高 | 分階段遷移 |

## 時程規劃
- 第1週：分片設計 + 路由層開發
- 第2週：測試環境驗證
- 第3週：雙寫模式上線
- 第4週：資料遷移
- 第5週：流量切換

## 成功標準
- 資料庫QPS容量提升3倍
- P99延遲維持在100ms以下
- 零資料遺失
- 停機時間少於1小時
```

#### 🔄 緊急變更流程

**生產環境緊急修復流程**：
```bash
#!/bin/bash
# hotfix.sh

ISSUE_ID=$1
DESCRIPTION=$2

if [ -z "$ISSUE_ID" ] || [ -z "$DESCRIPTION" ]; then
    echo "使用方法: $0 <ISSUE_ID> <DESCRIPTION>"
    echo "範例: $0 CRITICAL-123 '修復支付API異常'"
    exit 1
fi

echo "🚨 開始緊急修復流程"
echo "問題ID: $ISSUE_ID"
echo "描述: $DESCRIPTION"

# 1. 從生產分支建立hotfix分支
git checkout main
git pull origin main
HOTFIX_BRANCH="hotfix/$ISSUE_ID"
git checkout -b $HOTFIX_BRANCH

echo "✅ 建立hotfix分支: $HOTFIX_BRANCH"

# 2. 提醒開發者進行修復
echo "📝 請進行必要的程式碼修復..."
echo "完成後按Enter繼續，或Ctrl+C取消"
read

# 3. 驗證修復
echo "🧪 執行快速測試..."
npm run test:critical
if [ $? -ne 0 ]; then
    echo "❌ 關鍵測試失敗，請檢查修復"
    exit 1
fi

# 4. 提交修復
git add .
git commit -m "fix: $DESCRIPTION ($ISSUE_ID)"
git push origin $HOTFIX_BRANCH

# 5. 建立緊急PR
gh pr create \
    --title "🚨 HOTFIX: $DESCRIPTION" \
    --body "## 緊急修復

**問題ID**: $ISSUE_ID
**描述**: $DESCRIPTION

### 修復內容
- [ ] 問題原因分析
- [ ] 修復方案實施
- [ ] 測試驗證完成

### 部署檢查清單
- [ ] 程式碼審查 (簡化流程)
- [ ] 緊急測試通過
- [ ] 部署計畫確認
- [ ] 回復計畫準備

⚠️ 此為緊急修復，請優先處理" \
    --assignee "@me" \
    --label "hotfix,critical"

echo "🎯 緊急PR已建立，請盡快進行審查和部署"

# 6. 通知相關團隊
curl -X POST -H 'Content-type: application/json' \
    --data "{\"text\":\"🚨 緊急修復PR已建立\n問題: $DESCRIPTION\nPR: $(gh pr view --json url -q .url)\"}" \
    $SLACK_WEBHOOK_URL

echo "📢 已通知相關團隊"
```

### 3.4 技術債務管理

#### 📊 技術債務評估

**技術債務評估矩陣**：
```javascript
// technical-debt-analyzer.js
class TechnicalDebtAnalyzer {
  constructor() {
    this.metrics = {
      codeComplexity: 0,
      testCoverage: 0,
      duplication: 0,
      dependencies: 0,
      documentation: 0
    };
  }
  
  // 程式碼複雜度分析
  analyzeComplexity(filePath) {
    // 使用ESLint複雜度規則
    const complexity = this.calculateCyclomaticComplexity(filePath);
    
    return {
      score: this.normalizeScore(complexity, 1, 20, 100, 0),
      issues: complexity > 10 ? ['函數複雜度過高'] : [],
      recommendations: complexity > 10 ? ['考慮重構大型函數'] : []
    };
  }
  
  // 測試覆蓋率分析
  analyzeCoverage(coverageData) {
    const { statements, branches, functions, lines } = coverageData;
    const avgCoverage = (statements + branches + functions + lines) / 4;
    
    return {
      score: avgCoverage,
      issues: avgCoverage < 80 ? ['測試覆蓋率不足'] : [],
      recommendations: avgCoverage < 80 ? ['增加單元測試'] : []
    };
  }
  
  // 程式碼重複分析
  analyzeDuplication(jscpdReport) {
    const duplicationPercentage = jscpdReport.statistics.total.percentage;
    
    return {
      score: this.normalizeScore(duplicationPercentage, 0, 20, 100, 0),
      issues: duplicationPercentage > 5 ? ['程式碼重複率過高'] : [],
      recommendations: duplicationPercentage > 5 ? ['提取共用模組'] : []
    };
  }
  
  // 依賴分析
  analyzeDependencies(packageJson) {
    const deps = Object.keys(packageJson.dependencies || {});
    const devDeps = Object.keys(packageJson.devDependencies || {});
    const totalDeps = deps.length + devDeps.length;
    
    // 檢查過時依賴
    const outdatedCount = this.checkOutdatedDependencies(deps);
    
    return {
      score: this.normalizeScore(outdatedCount, 0, 10, 100, 0),
      issues: outdatedCount > 5 ? ['過多過時依賴'] : [],
      recommendations: outdatedCount > 0 ? ['更新依賴套件'] : []
    };
  }
  
  // 綜合評估
  generateReport() {
    const overallScore = Object.values(this.metrics)
      .reduce((sum, score) => sum + score, 0) / Object.keys(this.metrics).length;
    
    let level;
    if (overallScore >= 80) level = 'LOW';
    else if (overallScore >= 60) level = 'MEDIUM';
    else if (overallScore >= 40) level = 'HIGH';
    else level = 'CRITICAL';
    
    return {
      overallScore,
      debtLevel: level,
      priority: this.calculatePriority(level),
      estimatedEffort: this.estimateRefactoringEffort(level),
      recommendations: this.generateRecommendations()
    };
  }
  
  calculatePriority(level) {
    const priorities = {
      CRITICAL: 1,
      HIGH: 2,
      MEDIUM: 3,
      LOW: 4
    };
    return priorities[level];
  }
  
  estimateRefactoringEffort(level) {
    const efforts = {
      CRITICAL: '8-12週',
      HIGH: '4-6週',
      MEDIUM: '2-3週',
      LOW: '1週以內'
    };
    return efforts[level];
  }
  
  normalizeScore(value, min, max, targetMax, targetMin) {
    return ((value - min) / (max - min)) * (targetMax - targetMin) + targetMin;
  }
}

// 使用範例
const analyzer = new TechnicalDebtAnalyzer();
const report = analyzer.generateReport();
console.log('技術債務報告:', report);
```

---

## 🔒 第四部分：災難恢復與備份

### 4.1 災難恢復規劃

#### 🎯 RTO與RPO目標設定

**關鍵指標定義**：
- **RTO (Recovery Time Objective)**：從災難發生到服務恢復的最大容忍時間
- **RPO (Recovery Point Objective)**：可接受的最大資料遺失時間

**不同業務等級的要求**：

| 業務等級 | RTO | RPO | 成本 | 範例 |
|----------|-----|-----|------|------|
| **關鍵** | < 1小時 | < 15分鐘 | 極高 | 支付系統、交易平台 |
| **重要** | < 4小時 | < 1小時 | 高 | 用戶管理、訂單系統 |
| **一般** | < 24小時 | < 4小時 | 中等 | 報表系統、日誌分析 |
| **低** | < 72小時 | < 24小時 | 低 | 測試環境、文檔系統 |

#### 🗺️ 災難恢復策略

**冷備份 (Cold Backup)**：
```bash
#!/bin/bash
# cold-backup.sh

BACKUP_DIR="/backup/$(date +%Y%m%d)"
DATABASE_NAME="production_db"
S3_BUCKET="myapp-backups"

echo "開始冷備份流程..."

# 1. 建立備份目錄
mkdir -p $BACKUP_DIR

# 2. 資料庫備份
echo "備份資料庫..."
pg_dump $DATABASE_NAME | gzip > $BACKUP_DIR/database.sql.gz

# 3. 應用程式檔案備份
echo "備份應用程式檔案..."
tar -czf $BACKUP_DIR/application.tar.gz /opt/myapp

# 4. 配置檔案備份
echo "備份配置檔案..."
tar -czf $BACKUP_DIR/configs.tar.gz /etc/myapp

# 5. 上傳到雲端存儲
echo "上傳到S3..."
aws s3 sync $BACKUP_DIR s3://$S3_BUCKET/$(basename $BACKUP_DIR)

# 6. 驗證備份完整性
echo "驗證備份..."
if aws s3 ls s3://$S3_BUCKET/$(basename $BACKUP_DIR)/ | grep -q "database.sql.gz"; then
    echo "✅ 備份完成並驗證成功"
    
    # 清理本地備份 (保留7天)
    find /backup -type d -mtime +7 -exec rm -rf {} \;
else
    echo "❌ 備份驗證失敗"
    exit 1
fi
```

**熱備份 (Hot Backup) - PostgreSQL**：
```bash
#!/bin/bash
# hot-backup-postgres.sh

BACKUP_DIR="/backup/hot/$(date +%Y%m%d_%H%M%S)"
WAL_ARCHIVE_DIR="/backup/wal"

# 1. 開始基礎備份
echo "開始熱備份..."
psql -c "SELECT pg_start_backup('hot_backup_$(date +%Y%m%d_%H%M%S)');"

# 2. 複製資料檔案
mkdir -p $BACKUP_DIR
rsync -av --exclude='pg_wal/*' /var/lib/postgresql/data/ $BACKUP_DIR/

# 3. 結束基礎備份
psql -c "SELECT pg_stop_backup();"

# 4. 複製必要的WAL檔案
cp $WAL_ARCHIVE_DIR/* $BACKUP_DIR/pg_wal/

echo "✅ 熱備份完成: $BACKUP_DIR"
```

**主從複製配置**：
```sql
-- 主資料庫配置 (postgresql.conf)
wal_level = replica
max_wal_senders = 3
wal_keep_segments = 64
archive_mode = on
archive_command = 'cp %p /backup/wal/%f'

-- 創建複製用戶
CREATE USER replication_user REPLICATION LOGIN PASSWORD 'secure_password';

-- pg_hba.conf
host replication replication_user 192.168.1.100/32 md5
```

```bash
# 從資料庫設置
#!/bin/bash
# setup-slave.sh

MASTER_HOST="192.168.1.50"
SLAVE_DATA_DIR="/var/lib/postgresql/slave"

# 1. 停止從資料庫
systemctl stop postgresql-slave

# 2. 清理資料目錄
rm -rf $SLAVE_DATA_DIR/*

# 3. 從主資料庫復制基礎備份
pg_basebackup -h $MASTER_HOST -D $SLAVE_DATA_DIR -U replication_user -W -v -P

# 4. 配置恢復設置
cat > $SLAVE_DATA_DIR/recovery.conf << EOF
standby_mode = 'on'
primary_conninfo = 'host=$MASTER_HOST port=5432 user=replication_user password=secure_password'
recovery_target_timeline = 'latest'
EOF

# 5. 啟動從資料庫
systemctl start postgresql-slave

echo "✅ 從資料庫設置完成"
```

### 4.2 備份策略實施

#### 💾 3-2-1備份策略

**3-2-1原則**：
- **3**份副本：原始資料 + 2份備份
- **2**種媒體：不同類型的存儲媒體
- **1**份異地：至少1份備份在異地

**自動化備份腳本**：
```bash
#!/bin/bash
# automated-backup.sh

set -e

# 配置
APP_NAME="myapp"
BACKUP_ROOT="/backup"
LOCAL_BACKUP_DIR="$BACKUP_ROOT/local"
REMOTE_BACKUP_DIR="$BACKUP_ROOT/remote"
S3_BUCKET="myapp-disaster-recovery"
RETENTION_DAYS=30

# 日誌函數
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a /var/log/backup.log
}

# 備份資料庫
backup_database() {
    log "開始資料庫備份..."
    
    local backup_file="$LOCAL_BACKUP_DIR/db_$(date +%Y%m%d_%H%M%S).sql.gz"
    mkdir -p $(dirname $backup_file)
    
    pg_dump $APP_NAME | gzip > $backup_file
    
    if [ $? -eq 0 ]; then
        log "✅ 資料庫備份完成: $backup_file"
        echo $backup_file
    else
        log "❌ 資料庫備份失敗"
        exit 1
    fi
}

# 備份應用程式檔案
backup_application() {
    log "開始應用程式備份..."
    
    local backup_file="$LOCAL_BACKUP_DIR/app_$(date +%Y%m%d_%H%M%S).tar.gz"
    mkdir -p $(dirname $backup_file)
    
    tar -czf $backup_file \
        --exclude='node_modules' \
        --exclude='logs' \
        --exclude='.git' \
        /opt/$APP_NAME
    
    if [ $? -eq 0 ]; then
        log "✅ 應用程式備份完成: $backup_file"
        echo $backup_file
    else
        log "❌ 應用程式備份失敗"
        exit 1
    fi
}

# 上傳到雲端
upload_to_cloud() {
    local file=$1
    local filename=$(basename $file)
    
    log "上傳 $filename 到S3..."
    
    aws s3 cp $file s3://$S3_BUCKET/$(date +%Y/%m/%d)/$filename \
        --storage-class STANDARD_IA
    
    if [ $? -eq 0 ]; then
        log "✅ 雲端上傳完成: $filename"
    else
        log "❌ 雲端上傳失敗: $filename"
        return 1
    fi
}

# 複製到異地存儲
copy_to_remote() {
    local file=$1
    local remote_file="$REMOTE_BACKUP_DIR/$(basename $file)"
    
    log "複製到異地存儲..."
    mkdir -p $(dirname $remote_file)
    cp $file $remote_file
    
    if [ $? -eq 0 ]; then
        log "✅ 異地複製完成: $remote_file"
    else
        log "❌ 異地複製失敗"
        return 1
    fi
}

# 清理過期備份
cleanup_old_backups() {
    log "清理過期備份..."
    
    # 清理本地備份
    find $LOCAL_BACKUP_DIR -type f -mtime +$RETENTION_DAYS -delete
    
    # 清理異地備份
    find $REMOTE_BACKUP_DIR -type f -mtime +$RETENTION_DAYS -delete
    
    # 清理S3備份 (使用生命週期策略)
    log "✅ 過期備份清理完成"
}

# 驗證備份完整性
verify_backup() {
    local db_backup=$1
    local app_backup=$2
    
    log "驗證備份完整性..."
    
    # 驗證資料庫備份
    if gzip -t $db_backup; then
        log "✅ 資料庫備份檔案完整"
    else
        log "❌ 資料庫備份檔案損壞"
        return 1
    fi
    
    # 驗證應用程式備份
    if tar -tzf $app_backup > /dev/null; then
        log "✅ 應用程式備份檔案完整"
    else
        log "❌ 應用程式備份檔案損壞"
        return 1
    fi
}

# 主流程
main() {
    log "🚀 開始自動化備份流程"
    
    # 1. 備份資料庫和應用程式
    db_backup=$(backup_database)
    app_backup=$(backup_application)
    
    # 2. 驗證備份完整性
    verify_backup $db_backup $app_backup
    
    # 3. 實施3-2-1策略
    # 第1份：本地存儲 (已完成)
    # 第2份：異地存儲
    copy_to_remote $db_backup
    copy_to_remote $app_backup
    
    # 第3份：雲端存儲
    upload_to_cloud $db_backup
    upload_to_cloud $app_backup
    
    # 4. 清理過期備份
    cleanup_old_backups
    
    log "🎉 備份流程完成"
}

# 執行主流程
main

# 發送備份報告
if [ $? -eq 0 ]; then
    curl -X POST -H 'Content-type: application/json' \
        --data '{"text":"✅ 自動備份成功完成"}' \
        $SLACK_WEBHOOK_URL
else
    curl -X POST -H 'Content-type: application/json' \
        --data '{"text":"❌ 自動備份失敗，請檢查"}' \
        $SLACK_WEBHOOK_URL
fi
```

### 4.3 災難恢復演練

#### 🎭 恢復測試腳本

```bash
#!/bin/bash
# disaster-recovery-test.sh

TEST_ENV="dr-test"
BACKUP_DATE=${1:-$(date -d "yesterday" +%Y%m%d)}
S3_BUCKET="myapp-disaster-recovery"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# 恢復資料庫
restore_database() {
    log "開始恢復資料庫測試..."
    
    # 1. 下載備份檔案
    local backup_file="db_backup_$BACKUP_DATE.sql.gz"
    aws s3 cp s3://$S3_BUCKET/$BACKUP_DATE/$backup_file /tmp/
    
    # 2. 建立測試資料庫
    createdb ${TEST_ENV}_db
    
    # 3. 恢復資料
    gunzip -c /tmp/$backup_file | psql ${TEST_ENV}_db
    
    if [ $? -eq 0 ]; then
        log "✅ 資料庫恢復測試成功"
        return 0
    else
        log "❌ 資料庫恢復測試失敗"
        return 1
    fi
}

# 恢復應用程式
restore_application() {
    log "開始恢復應用程式測試..."
    
    # 1. 下載應用程式備份
    local backup_file="app_backup_$BACKUP_DATE.tar.gz"
    aws s3 cp s3://$S3_BUCKET/$BACKUP_DATE/$backup_file /tmp/
    
    # 2. 解壓到測試目錄
    mkdir -p /opt/${TEST_ENV}
    tar -xzf /tmp/$backup_file -C /opt/${TEST_ENV}
    
    # 3. 更新配置
    sed -i "s/production_db/${TEST_ENV}_db/g" /opt/${TEST_ENV}/config/database.yml
    
    if [ $? -eq 0 ]; then
        log "✅ 應用程式恢復測試成功"
        return 0
    else
        log "❌ 應用程式恢復測試失敗"
        return 1
    fi
}

# 功能驗證測試
verify_functionality() {
    log "開始功能驗證測試..."
    
    # 等待應用程式啟動
    sleep 30
    
    # 健康檢查
    if curl -f http://localhost:3001/health; then
        log "✅ 健康檢查通過"
    else
        log "❌ 健康檢查失敗"
        return 1
    fi
    
    # 關鍵功能測試
    local test_results=$(npm run test:critical -- --env=$TEST_ENV)
    if echo "$test_results" | grep -q "All tests passed"; then
        log "✅ 關鍵功能測試通過"
        return 0
    else
        log "❌ 關鍵功能測試失敗"
        return 1
    fi
}

# 清理測試環境
cleanup_test_environment() {
    log "清理測試環境..."
    
    # 停止測試應用程式
    pkill -f "${TEST_ENV}"
    
    # 刪除測試資料庫
    dropdb ${TEST_ENV}_db
    
    # 刪除測試檔案
    rm -rf /opt/${TEST_ENV}
    rm -f /tmp/*backup*
    
    log "✅ 測試環境清理完成"
}

# 生成恢復報告
generate_report() {
    local db_result=$1
    local app_result=$2
    local func_result=$3
    
    local report_file="/tmp/dr_test_report_$(date +%Y%m%d_%H%M%S).json"
    
    cat > $report_file << EOF
{
    "test_date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "backup_date": "$BACKUP_DATE",
    "results": {
        "database_restore": $([ $db_result -eq 0 ] && echo "true" || echo "false"),
        "application_restore": $([ $app_result -eq 0 ] && echo "true" || echo "false"),
        "functionality_test": $([ $func_result -eq 0 ] && echo "true" || echo "false")
    },
    "overall_success": $([ $db_result -eq 0 ] && [ $app_result -eq 0 ] && [ $func_result -eq 0 ] && echo "true" || echo "false"),
    "rto_achieved": "$(date -d "$start_time + 1 hour" +%H:%M:%S)",
    "recommendations": [
        "定期執行恢復測試",
        "優化備份檔案大小",
        "自動化恢復流程"
    ]
}
EOF
    
    log "恢復測試報告已生成: $report_file"
    
    # 上傳報告
    aws s3 cp $report_file s3://$S3_BUCKET/reports/
}

# 主要恢復測試流程
main() {
    local start_time=$(date)
    log "🚀 開始災難恢復測試 (備份日期: $BACKUP_DATE)"
    
    # 執行恢復測試
    restore_database
    local db_result=$?
    
    restore_application  
    local app_result=$?
    
    verify_functionality
    local func_result=$?
    
    # 生成報告
    generate_report $db_result $app_result $func_result
    
    # 清理環境
    cleanup_test_environment
    
    # 總結
    if [ $db_result -eq 0 ] && [ $app_result -eq 0 ] && [ $func_result -eq 0 ]; then
        log "🎉 災難恢復測試全部通過"
        return 0
    else
        log "❌ 災難恢復測試存在問題，請檢查"
        return 1
    fi
}

# 執行測試
main
```

---

## 📞 第五部分：使用者支援與文件管理

### 5.1 使用者支援體系

#### 🎯 支援層級設計

**多層級支援架構**：
```
第一層 (L1) - 基本支援
├── 常見問題解答
├── 自助服務工具
├── 聊天機器人
└── 基本問題處理

第二層 (L2) - 技術支援
├── 複雜問題診斷
├── 系統配置協助
├── 錯誤排除
└── 升級與更新

第三層 (L3) - 專家支援
├── 深度技術問題
├── 客製化開發
├── 架構諮詢
└── 緊急事件處理
```

#### 📊 支援指標與SLA

**服務水準協議 (SLA)**：

| 支援等級 | 回應時間 | 解決時間 | 可用性 | 成本 |
|----------|----------|----------|--------|------|
| **基本** | 24小時 | 5個工作日 | 99% | 低 |
| **標準** | 4小時 | 2個工作日 | 99.5% | 中 |
| **高級** | 1小時 | 8小時 | 99.9% | 高 |
| **企業** | 15分鐘 | 2小時 | 99.99% | 極高 |

### 5.2 文件管理體系

#### 📚 文件分類架構

**技術文件結構**：
```
技術文件庫
├── 使用者文件
│   ├── 使用者手冊
│   ├── 快速入門指南
│   ├── 常見問題FAQ
│   └── 影片教學
├── 開發者文件
│   ├── API文件
│   ├── SDK使用指南
│   ├── 架構設計文件
│   └── 開發環境設置
├── 運維文件
│   ├── 部署指南
│   ├── 監控設置
│   ├── 故障排除
│   └── 備份恢復
└── 流程文件
    ├── 開發流程
    ├── 測試流程
    ├── 發布流程
    └── 支援流程
```

#### 📝 API文件自動生成

**OpenAPI/Swagger文件生成**：
```javascript
// swagger-config.js
const swaggerJsdoc = require('swagger-jsdoc');
const swaggerUi = require('swagger-ui-express');

const options = {
  definition: {
    openapi: '3.0.0',
    info: {
      title: 'MyApp API',
      version: '1.0.0',
      description: 'MyApp API文件',
      contact: {
        name: 'API支援團隊',
        email: 'api-support@myapp.com'
      }
    },
    servers: [
      {
        url: 'https://api.myapp.com/v1',
        description: '生產環境'
      },
      {
        url: 'https://staging-api.myapp.com/v1', 
        description: '測試環境'
      }
    ],
    components: {
      securitySchemes: {
        bearerAuth: {
          type: 'http',
          scheme: 'bearer',
          bearerFormat: 'JWT'
        }
      }
    }
  },
  apis: ['./routes/*.js'], // API路由檔案路徑
};

const specs = swaggerJsdoc(options);

module.exports = { specs, swaggerUi };

// 使用範例 - routes/users.js
/**
 * @swagger
 * components:
 *   schemas:
 *     User:
 *       type: object
 *       required:
 *         - name
 *         - email
 *       properties:
 *         id:
 *           type: integer
 *           description: 使用者唯一識別碼
 *         name:
 *           type: string
 *           description: 使用者姓名
 *         email:
 *           type: string
 *           format: email
 *           description: 使用者電子郵件
 *         createdAt:
 *           type: string
 *           format: date-time
 *           description: 建立時間
 *       example:
 *         id: 1
 *         name: 張小明
 *         email: ming@example.com
 *         createdAt: 2024-03-15T10:30:00Z
 */

/**
 * @swagger
 * /users:
 *   get:
 *     summary: 取得使用者清單
 *     tags: [Users]
 *     security:
 *       - bearerAuth: []
 *     parameters:
 *       - in: query
 *         name: page
 *         schema:
 *           type: integer
 *           minimum: 1
 *           default: 1
 *         description: 頁碼
 *       - in: query
 *         name: limit
 *         schema:
 *           type: integer
 *           minimum: 1
 *           maximum: 100
 *           default: 20
 *         description: 每頁筆數
 *     responses:
 *       200:
 *         description: 成功取得使用者清單
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 users:
 *                   type: array
 *                   items:
 *                     $ref: '#/components/schemas/User'
 *                 totalPages:
 *                   type: integer
 *                 currentPage:
 *                   type: integer
 *                 total:
 *                   type: integer
 *       401:
 *         description: 未授權
 *       500:
 *         description: 伺服器錯誤
 */
router.get('/users', authenticateToken, async (req, res) => {
  try {
    const { page = 1, limit = 20 } = req.query;
    const users = await userService.getUsers(page, limit);
    res.json(users);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * @swagger
 * /users:
 *   post:
 *     summary: 建立新使用者
 *     tags: [Users]
 *     security:
 *       - bearerAuth: []
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - name
 *               - email
 *               - password
 *             properties:
 *               name:
 *                 type: string
 *                 example: 王小華
 *               email:
 *                 type: string
 *                 format: email
 *                 example: wang@example.com
 *               password:
 *                 type: string
 *                 minLength: 8
 *                 example: securePassword123
 *     responses:
 *       201:
 *         description: 使用者建立成功
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/User'
 *       400:
 *         description: 輸入資料無效
 *       409:
 *         description: 電子郵件已存在
 *       500:
 *         description: 伺服器錯誤
 */
router.post('/users', authenticateToken, async (req, res) => {
  try {
    const userData = req.body;
    const newUser = await userService.createUser(userData);
    res.status(201).json(newUser);
  } catch (error) {
    if (error.code === 'DUPLICATE_EMAIL') {
      res.status(409).json({ error: '電子郵件已存在' });
    } else {
      res.status(500).json({ error: error.message });
    }
  }
});
```

#### 📖 自動化文件發布

**文件CI/CD流程**：
```yaml
# .github/workflows/docs.yml
name: 文件發布

on:
  push:
    branches: [ main ]
    paths: 
      - 'docs/**'
      - 'routes/**'
      - 'swagger-config.js'

jobs:
  build-docs:
    runs-on: ubuntu-latest
    
    steps:
    - name: Checkout
      uses: actions/checkout@v3
    
    - name: 設定Node.js
      uses: actions/setup-node@v3
      with:
        node-version: 18
        cache: 'npm'
    
    - name: 安裝依賴
      run: npm ci
    
    - name: 生成API文件
      run: |
        npm run docs:generate
        npm run docs:build
    
    - name: 部署到GitHub Pages
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{ secrets.GITHUB_TOKEN }}
        publish_dir: ./docs/dist
    
    - name: 發送通知
      uses: 8398a7/action-slack@v3
      with:
        status: success
        text: '📚 API文件已更新: https://myapp.github.io/api-docs'
      env:
        SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK }}
```

---

## 🔄 第六部分：生命週期管理

### 6.1 軟體生命週期階段

#### 📈 產品成熟度曲線

```
效能/採用率
    ↑
    │     成熟期
    │    ┌─────────┐
    │   ╱           ╲
    │  ╱             ╲ 衰退期
    │ ╱ 成長期        ╲
    │╱                 ╲
引入期                  淘汰期
└────────────────────────→ 時間
```

**各階段特徵與策略**：

| 階段 | 特徵 | 策略重點 | 技術負債 | 維護重點 |
|------|------|----------|----------|----------|
| **引入期** | 功能基礎、使用者少 | 快速開發、功能驗證 | 可接受較高 | 錯誤修復 |
| **成長期** | 使用者快速增長 | 效能優化、穩定性 | 積極管理 | 擴展性 |
| **成熟期** | 市場穩定、功能完整 | 成本控制、效率 | 持續重構 | 安全性 |
| **衰退期** | 需求下降、競爭激烈 | 最小維護、遷移準備 | 凍結新增 | 必要修復 |

### 6.2 遺留系統管理

#### 🏛️ 遺留系統現代化策略

**現代化方法比較**：

| 方法 | 成本 | 風險 | 時間 | 適用情境 |
|------|------|------|------|----------|
| **保持現狀** | 低 | 高 | 短 | 即將淘汰的系統 |
| **封裝** | 低 | 中 | 短 | 需要快速整合 |
| **重新平台化** | 中 | 中 | 中 | 基礎設施老舊 |
| **重構** | 高 | 中 | 長 | 邏輯複雜但價值高 |
| **重建** | 極高 | 高 | 極長 | 完全不適用 |

**絞殺者模式 (Strangler Fig Pattern)**：
```javascript
// legacy-migration-proxy.js
const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');

const app = express();

// 配置路由映射
const routeConfig = {
  // 已遷移到新系統的路由
  '/api/users': 'http://new-service:3000',
  '/api/orders': 'http://new-service:3000',
  
  // 還在遺留系統的路由
  '/api/legacy/reports': 'http://legacy-system:8080',
  '/api/legacy/billing': 'http://legacy-system:8080'
};

// 特性開關配置
const featureFlags = {
  newUserService: { enabled: true, rollout: 100 },
  newOrderService: { enabled: true, rollout: 50 },
  newReportService: { enabled: false, rollout: 0 }
};

// 智能路由中間件
function intelligentRouter(req, res, next) {
  const path = req.path;
  const userId = req.headers['user-id'];
  
  // 檢查是否有對應的新服務
  for (const [route, target] of Object.entries(routeConfig)) {
    if (path.startsWith(route)) {
      // 檢查特性開關
      const featureName = getFeatureName(route);
      const feature = featureFlags[featureName];
      
      if (feature && feature.enabled) {
        // 根據推出比例決定路由
        const shouldUseNewService = shouldRouteToNewService(userId, feature.rollout);
        
        if (shouldUseNewService) {
          req.targetService = target;
          return next();
        }
      }
      
      // 路由到遺留系統
      req.targetService = routeConfig['/api/legacy' + route.replace('/api', '')];
      return next();
    }
  }
  
  // 預設路由到遺留系統
  req.targetService = 'http://legacy-system:8080';
  next();
}

function shouldRouteToNewService(userId, rolloutPercentage) {
  // 使用使用者ID的雜湊值確保一致性
  const hash = require('crypto').createHash('md5').update(userId).digest('hex');
  const hashValue = parseInt(hash.substring(0, 8), 16);
  return (hashValue % 100) < rolloutPercentage;
}

function getFeatureName(route) {
  const mapping = {
    '/api/users': 'newUserService',
    '/api/orders': 'newOrderService',
    '/api/reports': 'newReportService'
  };
  return mapping[route] || 'default';
}

// 動態代理中間件
app.use(intelligentRouter);

app.use('*', (req, res) => {
  const proxy = createProxyMiddleware({
    target: req.targetService,
    changeOrigin: true,
    onError: (err, req, res) => {
      console.error('代理錯誤:', err);
      res.status(500).json({ error: '服務暫時不可用' });
    },
    onProxyReq: (proxyReq, req, res) => {
      // 添加追蹤標頭
      proxyReq.setHeader('X-Forwarded-Service', req.targetService);
      proxyReq.setHeader('X-Migration-Phase', 'strangler-pattern');
    }
  });
  
  proxy(req, res);
});

app.listen(3000, () => {
  console.log('遷移代理服務器運行在端口 3000');
});
```

### 6.3 軟體終止計畫

#### 🏁 系統退役流程

**退役計畫檢查清單**：
```markdown
# 軟體系統退役計畫

## 退役決策
- [ ] 業務需求評估
- [ ] 成本效益分析
- [ ] 替代方案確認
- [ ] 利害關係人同意

## 資料處理
- [ ] 資料備份策略
- [ ] 資料遷移計畫
- [ ] 資料保留政策
- [ ] 資料銷毀程序

## 使用者溝通
- [ ] 退役通知計畫
- [ ] 使用者培訓
- [ ] 支援服務過渡
- [ ] 文件更新

## 技術準備
- [ ] 相依性分析
- [ ] 服務停止計畫
- [ ] 資源回收
- [ ] 授權管理

## 法規合規
- [ ] 法規要求檢查
- [ ] 稽核準備
- [ ] 記錄保存
- [ ] 報告義務
```

**自動化退役腳本**：
```bash
#!/bin/bash
# system-decommission.sh

SYSTEM_NAME=$1
DECOMMISSION_DATE=$2

if [ -z "$SYSTEM_NAME" ] || [ -z "$DECOMMISSION_DATE" ]; then
    echo "使用方法: $0 <系統名稱> <退役日期>"
    exit 1
fi

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "/var/log/decommission_${SYSTEM_NAME}.log"
}

# 階段1：準備階段
prepare_decommission() {
    log "開始系統退役準備：$SYSTEM_NAME"
    
    # 1. 建立最終備份
    log "建立最終系統備份..."
    ./backup-system.sh $SYSTEM_NAME final
    
    # 2. 匯出配置
    log "匯出系統配置..."
    kubectl get all -n $SYSTEM_NAME -o yaml > "${SYSTEM_NAME}_final_config.yaml"
    
    # 3. 生成服務清單
    log "生成服務依賴清單..."
    kubectl get services -n $SYSTEM_NAME -o json | jq '.items[].metadata.name' > "${SYSTEM_NAME}_services.txt"
    
    # 4. 通知監控系統
    log "更新監控系統..."
    curl -X POST http://monitoring-api/systems/$SYSTEM_NAME/decommission \
        -H "Content-Type: application/json" \
        -d "{\"date\":\"$DECOMMISSION_DATE\",\"status\":\"preparing\"}"
}

# 階段2：服務停止
stop_services() {
    log "開始停止服務..."
    
    # 1. 停止新請求
    log "停止負載平衡器流量..."
    kubectl patch service $SYSTEM_NAME-service -p '{"spec":{"selector":{"app":"stopped"}}}'
    
    # 2. 等待現有請求完成
    log "等待現有請求完成 (30秒)..."
    sleep 30
    
    # 3. 停止應用程式
    log "停止應用程式..."
    kubectl scale deployment $SYSTEM_NAME --replicas=0
    
    # 4. 停止資料庫
    log "停止資料庫..."
    kubectl scale statefulset $SYSTEM_NAME-db --replicas=0
    
    log "✅ 所有服務已停止"
}

# 階段3：資料處理
handle_data() {
    log "開始資料處理..."
    
    # 1. 最終資料匯出
    log "匯出最終資料..."
    pg_dump $SYSTEM_NAME | gzip > "${SYSTEM_NAME}_final_data.sql.gz"
    
    # 2. 資料驗證
    log "驗證資料完整性..."
    if gzip -t "${SYSTEM_NAME}_final_data.sql.gz"; then
        log "✅ 資料備份驗證成功"
    else
        log "❌ 資料備份驗證失敗"
        exit 1
    fi
    
    # 3. 上傳到長期儲存
    log "上傳到長期儲存..."
    aws s3 cp "${SYSTEM_NAME}_final_data.sql.gz" "s3://long-term-storage/decommissioned/${SYSTEM_NAME}/"
    
    # 4. 資料保留標記
    log "設定資料保留標記..."
    aws s3api put-object-tagging \
        --bucket long-term-storage \
        --key "decommissioned/${SYSTEM_NAME}/${SYSTEM_NAME}_final_data.sql.gz" \
        --tagging 'TagSet=[{Key=RetentionPeriod,Value=7years},{Key=System,Value='$SYSTEM_NAME'}]'
}

# 階段4：資源清理
cleanup_resources() {
    log "開始資源清理..."
    
    # 1. 刪除Kubernetes資源
    log "刪除Kubernetes資源..."
    kubectl delete namespace $SYSTEM_NAME
    
    # 2. 清理負載平衡器
    log "清理負載平衡器..."
    aws elbv2 delete-load-balancer --load-balancer-arn $(aws elbv2 describe-load-balancers --names $SYSTEM_NAME --query 'LoadBalancers[0].LoadBalancerArn' --output text)
    
    # 3. 刪除資料庫實例
    log "刪除資料庫實例..."
    aws rds delete-db-instance --db-instance-identifier $SYSTEM_NAME --final-db-snapshot-identifier "${SYSTEM_NAME}-final-snapshot"
    
    # 4. 清理監控配置
    log "清理監控配置..."
    curl -X DELETE http://monitoring-api/systems/$SYSTEM_NAME
    
    log "✅ 資源清理完成"
}

# 階段5：文件歸檔
archive_documentation() {
    log "歸檔系統文件..."
    
    # 1. 收集所有文件
    mkdir -p "/archive/${SYSTEM_NAME}"
    cp -r "/docs/${SYSTEM_NAME}" "/archive/${SYSTEM_NAME}/docs"
    cp "${SYSTEM_NAME}_final_config.yaml" "/archive/${SYSTEM_NAME}/"
    cp "${SYSTEM_NAME}_services.txt" "/archive/${SYSTEM_NAME}/"
    
    # 2. 建立歸檔清單
    find "/archive/${SYSTEM_NAME}" -type f > "/archive/${SYSTEM_NAME}/archive_manifest.txt"
    
    # 3. 上傳歸檔
    tar -czf "${SYSTEM_NAME}_archive.tar.gz" "/archive/${SYSTEM_NAME}"
    aws s3 cp "${SYSTEM_NAME}_archive.tar.gz" "s3://document-archive/decommissioned/"
    
    log "✅ 文件歸檔完成"
}

# 階段6：最終報告
generate_final_report() {
    log "生成退役報告..."
    
    cat > "${SYSTEM_NAME}_decommission_report.md" << EOF
# ${SYSTEM_NAME} 系統退役報告

## 基本資訊
- **系統名稱**: ${SYSTEM_NAME}
- **退役日期**: ${DECOMMISSION_DATE}
- **執行人員**: $(whoami)
- **退役原因**: [請填入]

## 執行摘要
- ✅ 系統備份完成
- ✅ 服務正常停止
- ✅ 資料安全匯出
- ✅ 資源完全清理
- ✅ 文件完整歸檔

## 保留資源
- **資料備份**: s3://long-term-storage/decommissioned/${SYSTEM_NAME}/
- **文件歸檔**: s3://document-archive/decommissioned/${SYSTEM_NAME}_archive.tar.gz
- **最終快照**: ${SYSTEM_NAME}-final-snapshot

## 注意事項
- 資料保留期限：7年
- 歸檔文件訪問：需要申請
- 緊急恢復：聯繫系統管理員

## 簽核
- 系統管理員: ________________
- 業務負責人: ________________
- 資訊安全: ________________
EOF

    log "✅ 退役報告已生成"
}

# 主流程
main() {
    log "🚀 開始系統退役流程：$SYSTEM_NAME"
    
    # 確認退役
    echo "⚠️  即將退役系統：$SYSTEM_NAME"
    echo "退役日期：$DECOMMISSION_DATE"
    echo "此操作不可逆，請確認 (輸入 'CONFIRM' 繼續):"
    read confirmation
    
    if [ "$confirmation" != "CONFIRM" ]; then
        log "❌ 退役操作已取消"
        exit 1
    fi
    
    # 執行退役步驟
    prepare_decommission
    stop_services
    handle_data
    cleanup_resources
    archive_documentation
    generate_final_report
    
    log "🎉 系統退役完成：$SYSTEM_NAME"
    
    # 發送通知
    curl -X POST -H 'Content-type: application/json' \
        --data "{\"text\":\"✅ 系統 ${SYSTEM_NAME} 已成功退役\"}" \
        $SLACK_WEBHOOK_URL
}

# 執行主流程
main
```

---

## 🏃‍♂️ 實務練習

### 練習1：部署策略設計與實作

**專案背景**：
為「線上學習平台」設計完整的多環境部署策略。

**系統架構**：
- 前端：React SPA
- 後端：Node.js API
- 資料庫：PostgreSQL
- 快取：Redis
- 檔案儲存：AWS S3

**任務1.1：環境架構設計**
設計四個環境的完整架構：
1. **開發環境 (Development)**
   - 本地Docker Compose配置
   - 熱重載開發環境
   - 模擬數據設定

2. **測試環境 (Testing)**
   - 自動化測試執行環境
   - CI/CD整合配置
   - 測試數據管理

3. **預產環境 (Staging)**
   - 生產環境完全複製
   - 效能測試環境
   - 使用者驗收測試

4. **生產環境 (Production)**
   - 高可用性配置
   - 負載平衡設計
   - 監控告警設置

**任務1.2：部署流程實作**
實作三種部署策略：

1. **藍綠部署腳本**
```bash
#!/bin/bash
# 建立藍綠部署自動化腳本
# 要求：零停機、快速回復、健康檢查
```

2. **金絲雀部署配置**
```yaml
# 使用Istio或NGINX實作金絲雀部署
# 要求：流量分配、指標監控、自動回復
```

3. **滾動更新策略**
```yaml
# Kubernetes滾動更新配置
# 要求：平滑更新、資源控制、失敗處理
```

**任務1.3：CI/CD Pipeline建立**
建立完整的GitHub Actions工作流：
- 程式碼檢查和測試
- Docker映像建置
- 安全掃描
- 多環境部署
- 部署驗證

**提交成果**：
- 多環境架構設計圖
- 完整的部署腳本
- CI/CD Pipeline配置
- 部署手冊文件

### 練習2：監控與告警系統建立

**任務2.1：Prometheus監控設置**
為學習平台建立完整監控系統：

1. **應用層指標**
   - HTTP請求指標
   - 業務指標（註冊數、課程完成率）
   - 錯誤率和延遲指標

2. **基礎設施指標**
   - 系統資源使用率
   - 資料庫效能指標
   - 網路流量指標

3. **Grafana儀表板**
   - 系統概覽儀表板
   - 應用程式效能儀表板
   - 業務指標儀表板

**任務2.2：告警規則設計**
設計分層告警系統：

```yaml
# 範例告警規則結構
groups:
- name: critical-alerts
  rules:
  - alert: ServiceDown
    expr: up == 0
    for: 1m
    labels:
      severity: critical
    annotations:
      summary: "服務停止運行"
      
- name: performance-alerts
  rules:
  - alert: HighResponseTime
    expr: histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m])) > 1
    for: 5m
    labels:
      severity: warning
```

**任務2.3：日誌管理系統**
建立ELK Stack日誌管理：
- Elasticsearch集群設置
- Logstash日誌處理
- Kibana視覺化配置
- 日誌輪轉和保留策略

**提交成果**：
- Prometheus配置檔案
- Grafana儀表板定義
- Alertmanager告警配置
- ELK Stack部署配置
- 監控運維手冊

### 練習3：災難恢復計畫

**情境設定**：
模擬生產環境發生重大故障，需要執行災難恢復。

**任務3.1：備份策略實施**
建立完整的3-2-1備份策略：

1. **自動化備份腳本**
```bash
#!/bin/bash
# 實作每日、每週、每月備份策略
# 包含資料庫、應用程式、配置檔案
```

2. **備份驗證機制**
```bash
#!/bin/bash
# 定期驗證備份完整性
# 自動化恢復測試
```

3. **雲端備份設置**
- AWS S3跨區域複製
- 生命週期管理策略
- 存取控制設定

**任務3.2：災難恢復演練**
設計並執行災難恢復演練：

1. **RTO/RPO目標設定**
   - 關鍵服務：RTO < 1小時，RPO < 15分鐘
   - 一般服務：RTO < 4小時，RPO < 1小時

2. **恢復程序文件**
   - 詳細恢復步驟
   - 角色與責任分工
   - 溝通和升級流程

3. **演練腳本**
```bash
#!/bin/bash
# 災難恢復演練自動化腳本
# 包含環境重建、資料恢復、服務驗證
```

**任務3.3：業務連續性計畫**
制定完整的業務連續性計畫：
- 風險評估和影響分析
- 備援策略和替代方案
- 溝通計畫和利害關係人管理
- 定期審查和更新機制

**提交成果**：
- 備份策略文件
- 災難恢復手冊
- 演練報告和改善建議
- 業務連續性計畫

### 練習4：使用者支援體系建立

**任務4.1：多層級支援設計**
建立完整的使用者支援體系：

1. **自助服務門戶**
   - 知識庫建立
   - 常見問題FAQ
   - 影片教學庫
   - 支援工單系統

2. **聊天機器人**
```javascript
// 建立智能客服機器人
// 常見問題自動回覆
// 問題分類和路由
```

3. **支援流程設計**
   - 問題分級處理
   - SLA時間設定
   - 升級機制設計

**任務4.2：API文件系統**
建立完整的技術文件體系：

1. **OpenAPI文件生成**
```javascript
// 自動化API文件生成
// 包含範例、錯誤碼、認證說明
```

2. **互動式文件**
   - Swagger UI設置
   - API測試功能
   - SDK使用範例

3. **文件發布流程**
```yaml
# CI/CD自動化文件發布
# 版本控制和歷史保存
```

**任務4.3：支援指標追蹤**
建立支援品質指標系統：
- 回應時間監控
- 解決率統計
- 使用者滿意度調查
- 支援成本分析

**提交成果**：
- 支援體系設計文件
- 自助服務門戶原型
- API文件系統
- 支援指標儀表板

---

## 📝 知識檢核測驗

### 選擇題 (每題5分，共50分)

**1. 藍綠部署的主要優勢是什麼？**
A) 節省伺服器成本
B) 實現零停機部署
C) 簡化開發流程
D) 減少程式碼複雜度

**2. RTO和RPO分別代表什麼？**
A) 修復時間和復原點
B) 運行時間和效能點
C) 回應時間和處理點
D) 可靠時間和效率點

**3. 3-2-1備份策略中的"3"指的是什麼？**
A) 3個備份位置
B) 3種備份方法
C) 3份資料副本
D) 3天備份週期

**4. Prometheus主要用於什麼？**
A) 程式碼版本控制
B) 系統監控和告警
C) 自動化部署
D) 資料庫管理

**5. 金絲雀部署的核心概念是什麼？**
A) 完全替換舊版本
B) 同時運行兩個版本
C) 漸進式流量分配
D) 定時自動更新

**6. 災難恢復演練的主要目的是什麼？**
A) 測試備份完整性
B) 驗證恢復流程
C) 檢查系統效能
D) 訓練技術人員

**7. 技術債務管理的首要原則是什麼？**
A) 立即修復所有問題
B) 根據影響和優先級處理
C) 忽略遺留系統問題
D) 等待系統重寫

**8. SLA中最重要的指標通常是什麼？**
A) 系統功能數量
B) 可用性百分比
C) 使用者數量
D) 程式碼品質

**9. 絞殺者模式主要用於什麼場景？**
A) 新系統開發
B) 效能優化
C) 遺留系統現代化
D) 安全性加強

**10. 軟體維護成本通常占整個生命週期成本的多少？**
A) 30%
B) 50%
C) 70%
D) 90%

### 簡答題 (每題25分，共50分)

**1. 請說明藍綠部署、金絲雀部署和滾動更新三種部署策略的差異，並分析各自的適用場景和優缺點。(300字)**

**2. 描述一個完整的災難恢復計畫應該包含哪些要素？如何確保災難恢復計畫的有效性？請提供具體的實施建議。(300字)**

---

## 📚 延伸學習資源

### 必讀書籍
1. **《Site Reliability Engineering》** - Google SRE Team
2. **《The DevOps Handbook》** - Gene Kim, Jez Humble
3. **《Continuous Delivery》** - Jez Humble, David Farley
4. **《Infrastructure as Code》** - Kief Morris
5. **《Monitoring and Observability》** - Cindy Sridharan

### 線上課程
1. **AWS Solutions Architect** - 雲端架構與部署
2. **Kubernetes Administrator (CKA)** - 容器編排管理
3. **Prometheus Monitoring** - 系統監控實務
4. **Disaster Recovery Planning** - 災難恢復規劃

### 實用工具
1. **部署工具**：
   - Kubernetes - 容器編排平台
   - Helm - Kubernetes套件管理
   - ArgoCD - GitOps持續部署
   - Spinnaker - 多雲部署平台

2. **監控工具**：
   - Prometheus - 監控和告警
   - Grafana - 視覺化儀表板
   - Jaeger - 分散式追蹤
   - ELK Stack - 日誌管理

3. **基礎設施工具**：
   - Terraform - 基礎設施即代碼
   - Ansible - 配置管理
   - Packer - 映像建置
   - Vault - 秘密管理

4. **雲端平台**：
   - AWS - 亞馬遜雲端服務
   - Azure - 微軟雲端平台
   - GCP - Google雲端平台
   - DigitalOcean - 簡化雲端服務

### 認證與社群
1. **專業認證**：
   - AWS Certified Solutions Architect
   - Certified Kubernetes Administrator
   - Google Cloud Professional DevOps Engineer
   - Microsoft Azure DevOps Engineer

2. **技術社群**：
   - CNCF (Cloud Native Computing Foundation)
   - DevOps台灣社群
   - SRE Taiwan
   - Kubernetes Taiwan User Group

---

## ✅ 學習檢核表

- [ ] 理解現代軟體部署策略和最佳實務
- [ ] 掌握多環境部署架構設計
- [ ] 熟悉藍綠、金絲雀、滾動更新部署方法
- [ ] 能建立完整的CI/CD自動化流程
- [ ] 了解系統監控和告警機制
- [ ] 掌握Prometheus和Grafana使用
- [ ] 能設計有效的備份和恢復策略
- [ ] 理解災難恢復計畫的重要性
- [ ] 了解技術債務管理方法
- [ ] 能建立使用者支援體系
- [ ] 掌握API文件自動化生成
- [ ] 理解軟體生命週期管理
- [ ] 完成所有實務練習
- [ ] 通過知識檢核測驗 (70分以上)

---

## 🎓 課程總結與展望

### 📊 學習歷程回顧

恭喜您完成了完整的軟體開發生命週期 (SDLC) 自主訓練課程！讓我們回顧這8週的學習歷程：

**第一模組：SDLC基礎概念** ✅
- 建立軟體工程思維
- 理解開發過程的複雜性
- 認識品質保證的重要性

**第二模組：SDLC模型與方法論** ✅
- 掌握傳統與敏捷開發模型
- 學會選擇適合的開發方法
- 建立DevOps文化認知

**第三模組：需求分析與規格撰寫** ✅
- 掌握需求工程核心技能
- 學會撰寫專業需求文件
- 建立需求管理流程

**第四模組：系統設計與架構** ✅
- 學會系統化設計思維
- 掌握UML建模技巧
- 建立架構決策能力

**第五模組：程式開發與實作** ✅
- 建立專業編碼標準
- 掌握版本控制最佳實務
- 學會程式碼品質管理

**第六模組：軟體測試** ✅
- 建立全面測試思維
- 掌握自動化測試技能
- 學會TDD開發方法

**第七模組：部署與維護** ✅
- 掌握現代部署策略
- 建立監控和維護體系
- 學會災難恢復規劃

### 🚀 核心能力獲得

通過這個課程，您已經具備了：

**技術能力**：
- 完整的SDLC流程掌握
- 現代開發工具鏈使用
- 自動化測試和部署技能
- 系統監控和維護能力

**軟技能**：
- 系統化思考能力
- 問題分析和解決技巧
- 團隊協作和溝通能力
- 持續學習和改善意識

**管理能力**：
- 專案規劃和執行
- 風險識別和控制
- 品質保證和流程優化
- 團隊指導和知識傳承

### 🎯 職涯發展路徑

基於您所學的技能，可以考慮以下職涯發展方向：

**技術專家路線**：
- **資深軟體工程師** → **技術架構師** → **首席技術官**
- **重點技能**：深度技術專精、架構設計、技術決策

**產品開發路線**：
- **全端開發工程師** → **產品經理** → **產品總監**
- **重點技能**：跨領域整合、使用者思維、產品策略

**質量保證路線**：
- **測試工程師** → **QA主管** → **質量總監**
- **重點技能**：測試策略、流程優化、品質文化

**運維管理路線**：
- **DevOps工程師** → **SRE工程師** → **技術運營總監**
- **重點技能**：自動化、監控、可靠性工程

### 📈 持續學習建議

**短期目標 (3-6個月)**：
- 在實際專案中應用所學技能
- 深化特定領域的專業知識
- 參與開源專案貢獻
- 取得相關技術認證

**中期目標 (1-2年)**：
- 建立個人技術品牌
- 成為團隊中的技術導師
- 跨領域知識整合
- 領導小型專案

**長期目標 (3-5年)**：
- 發展架構師或管理能力
- 建立行業影響力
- 創新技術應用
- 培養下一代技術人才

### 🌟 學習心態與方法

**保持好奇心**：
- 持續關注技術趨勢
- 嘗試新工具和方法
- 參與技術社群討論
- 定期反思和調整

**實踐導向**：
- 將理論應用到實際專案
- 建立個人專案作品集
- 記錄學習過程和心得
- 分享經驗幫助他人

**系統思考**：
- 從整體角度看待問題
- 理解各個環節的關聯
- 平衡技術和業務需求
- 考慮長期維護成本

---

## 🔮 未來技術趨勢

### 🤖 值得關注的技術方向

**人工智慧與機器學習**：
- AI輔助軟體開發
- 智能測試和除錯
- 自動化程式碼生成
- 預測性維護

**雲原生技術**：
- Serverless架構
- 微服務治理
- 服務網格 (Service Mesh)
- 邊緣計算

**低程式碼/無程式碼**：
- 視覺化開發平台
- API優先設計
- 公民開發者賦能
- 快速原型開發

**安全與隱私**：
- DevSecOps整合
- 零信任架構
- 隱私保護技術
- 合規自動化

### 🎓 推薦深度學習領域

**選擇建議**：根據個人興趣和職涯目標，建議選擇1-2個領域進行深度學習：

1. **雲端架構與DevOps**
2. **前端技術與使用者體驗**
3. **後端系統與資料工程**
4. **行動應用與跨平台開發**
5. **人工智慧與機器學習**
6. **資訊安全與隱私保護**

---

## 💬 結語與感謝

感謝您完成這個完整的SDLC自主訓練課程！這8週的學習歷程不僅是知識的累積，更是思維方式的轉變和專業能力的提升。

**記住**：
- 軟體開發是一個不斷學習和進步的過程
- 理論與實務的結合是成功的關鍵
- 團隊協作比個人技能更重要
- 持續改善是專業成長的動力

**下一步行動建議**：
1. 選擇一個實際專案應用所學技能
2. 加入技術社群分享學習心得
3. 制定個人學習和職涯發展計畫
4. 持續關注技術趨勢和最佳實務

**最後的祝福**：
願您在軟體開發的道路上持續成長，用技術改變世界，用代碼創造價值！

## 🌟 學習反思

請花15分鐘完成最終反思：

1. **最大收穫**：這8週學習中最寶貴的收穫是什麼？
2. **實務應用**：您計畫如何將所學應用到實際工作中？
3. **技能缺口**：還有哪些技能需要進一步加強？
4. **職涯規劃**：未來1-2年的學習和職涯目標是什麼？
5. **持續改善**：如何建立持續學習和改善的習慣？

---

**🎉 再次恭喜您完成SDLC自主訓練課程！**  
*您現在已經具備了現代軟體開發的完整技能，準備好在技術領域發光發熱了！*

**祝您在軟體開發的道路上一帆風順，前程似錦！** 🚀✨