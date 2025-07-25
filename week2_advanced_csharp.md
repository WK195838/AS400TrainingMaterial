# 第二週：進階C#特性和集合 - 完整教材

## 📚 本週學習目標

完成本週學習後，學員將能夠：
- 理解和運用泛型程式設計的概念和優勢
- 熟練使用各種C#集合類型並選擇最適合的集合
- 掌握LINQ查詢語法進行資料處理和轉換
- 理解Lambda表達式和函數式程式設計概念
- 建立健全的異常處理機制
- 運用委派和事件實作事件驅動程式設計

---

## 🔧 第一節：泛型程式設計

### 1.1 泛型基礎概念

#### 1.1.1 為什麼需要泛型？

```csharp
using System;
using System.Collections;
using System.Collections.Generic;

namespace GenericBasics
{
    // 問題：不使用泛型的情況
    public class NonGenericStack
    {
        private object[] _items = new object[10];
        private int _count = 0;
        
        public void Push(object item)
        {
            if (_count >= _items.Length)
                throw new InvalidOperationException("Stack is full");
            _items[_count++] = item;
        }
        
        public object Pop()
        {
            if (_count == 0)
                throw new InvalidOperationException("Stack is empty");
            return _items[--_count];
        }
        
        // 問題：
        // 1. 類型安全問題 - 可以放入任何類型的物件
        // 2. 效能問題 - 值類型需要裝箱和拆箱
        // 3. 程式碼可讀性差 - 需要強制轉型
    }
    
    // 解決方案：使用泛型
    public class GenericStack<T>
    {
        private T[] _items = new T[10];
        private int _count = 0;
        
        public void Push(T item)
        {
            if (_count >= _items.Length)
                ResizeArray();
            _items[_count++] = item;
        }
        
        public T Pop()
        {
            if (_count == 0)
                throw new InvalidOperationException("Stack is empty");
            return _items[--_count];
        }
        
        public T Peek()
        {
            if (_count == 0)
                throw new InvalidOperationException("Stack is empty");
            return _items[_count - 1];
        }
        
        public int Count => _count;
        public bool IsEmpty => _count == 0;
        
        private void ResizeArray()
        {
            T[] newArray = new T[_items.Length * 2];
            Array.Copy(_items, newArray, _items.Length);
            _items = newArray;
        }
        
        // 優點：
        // 1. 類型安全 - 編譯時期檢查
        // 2. 效能提升 - 避免裝箱拆箱
        // 3. 程式碼重用 - 一套程式碼支援多種類型
    }
    
    class GenericDemo
    {
        static void Main()
        {
            Console.WriteLine("=== 泛型Stack示範 ===");
            
            // 整數Stack
            var intStack = new GenericStack<int>();
            intStack.Push(1);
            intStack.Push(2);
            intStack.Push(3);
            
            Console.WriteLine($"整數Stack - Count: {intStack.Count}");
            while (!intStack.IsEmpty)
            {
                Console.WriteLine($"Pop: {intStack.Pop()}");
            }
            
            // 字串Stack
            var stringStack = new GenericStack<string>();
            stringStack.Push("First");
            stringStack.Push("Second");
            stringStack.Push("Third");
            
            Console.WriteLine($"\n字串Stack - Count: {stringStack.Count}");
            Console.WriteLine($"Peek: {stringStack.Peek()}"); // 不移除元素
            while (!stringStack.IsEmpty)
            {
                Console.WriteLine($"Pop: {stringStack.Pop()}");
            }
            
            // 自訂類型Stack
            var personStack = new GenericStack<Person>();
            personStack.Push(new Person("Alice", 30));
            personStack.Push(new Person("Bob", 25));
            
            Console.WriteLine($"\nPerson Stack - Count: {personStack.Count}");
            while (!personStack.IsEmpty)
            {
                var person = personStack.Pop();
                Console.WriteLine($"Pop: {person.Name}, Age: {person.Age}");
            }
        }
    }
    
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        
        public Person(string name, int age)
        {
            Name = name;
            Age = age;
        }
    }
}
```

#### 1.1.2 泛型約束

```csharp
using System;
using System.Collections.Generic;

namespace GenericConstraints
{
    // 基本介面約束
    public interface IComparable<T>
    {
        int CompareTo(T other);
    }
    
    public interface IDrawable
    {
        void Draw();
    }
    
    // 類別約束範例
    public abstract class Shape : IDrawable
    {
        public abstract double Area { get; }
        public abstract void Draw();
    }
    
    public class Circle : Shape
    {
        public double Radius { get; set; }
        public override double Area => Math.PI * Radius * Radius;
        
        public Circle(double radius)
        {
            Radius = radius;
        }
        
        public override void Draw()
        {
            Console.WriteLine($"Drawing a circle with radius {Radius:F2}");
        }
    }
    
    public class Rectangle : Shape
    {
        public double Width { get; set; }
        public double Height { get; set; }
        public override double Area => Width * Height;
        
        public Rectangle(double width, double height)
        {
            Width = width;
            Height = height;
        }
        
        public override void Draw()
        {
            Console.WriteLine($"Drawing a rectangle {Width}x{Height}");
        }
    }
    
    // 泛型約束示範
    public class GenericConstraintExamples
    {
        // 1. where T : class - 參考型別約束
        public class ReferenceTypeContainer<T> where T : class
        {
            private T _value;
            
            public void SetValue(T value)
            {
                _value = value ?? throw new ArgumentNullException(nameof(value));
            }
            
            public T GetValue() => _value;
            
            public bool HasValue() => _value != null;
        }
        
        // 2. where T : struct - 值型別約束
        public class ValueTypeContainer<T> where T : struct
        {
            private T? _value;
            
            public void SetValue(T value)
            {
                _value = value;
            }
            
            public T GetValueOrDefault(T defaultValue = default(T))
            {
                return _value ?? defaultValue;
            }
        }
        
        // 3. where T : new() - 預設建構函式約束
        public class Factory<T> where T : new()
        {
            public T CreateInstance()
            {
                return new T();
            }
            
            public List<T> CreateInstances(int count)
            {
                var instances = new List<T>();
                for (int i = 0; i < count; i++)
                {
                    instances.Add(new T());
                }
                return instances;
            }
        }
        
        // 4. where T : BaseClass - 基底類別約束
        public class ShapeProcessor<T> where T : Shape
        {
            private List<T> _shapes = new List<T>();
            
            public void AddShape(T shape)
            {
                _shapes.Add(shape);
            }
            
            public void DrawAllShapes()
            {
                Console.WriteLine("Drawing all shapes:");
                foreach (var shape in _shapes)
                {
                    shape.Draw(); // 可以呼叫Shape的方法
                }
            }
            
            public double CalculateTotalArea()
            {
                double total = 0;
                foreach (var shape in _shapes)
                {
                    total += shape.Area; // 可以存取Shape的屬性
                }
                return total;
            }
        }
        
        // 5. where T : interface - 介面約束
        public class DrawableCollection<T> where T : IDrawable
        {
            private List<T> _items = new List<T>();
            
            public void Add(T item)
            {
                _items.Add(item);
            }
            
            public void DrawAll()
            {
                foreach (var item in _items)
                {
                    item.Draw(); // 保證有Draw方法
                }
            }
        }
        
        // 6. 多重約束
        public class ComplexConstraint<T> where T : Shape, IComparable<T>, new()
        {
            public T CreateAndCompare(T other)
            {
                var newInstance = new T(); // new()約束
                newInstance.Draw(); // Shape約束
                
                int comparison = newInstance.CompareTo(other); // IComparable<T>約束
                Console.WriteLine($"Comparison result: {comparison}");
                
                return newInstance;
            }
        }
        
        // 7. 兩個類型參數的約束
        public class Converter<TInput, TOutput> 
            where TInput : class 
            where TOutput : class, new()
        {
            public TOutput Convert(TInput input, Func<TInput, TOutput> converter)
            {
                if (input == null)
                    return new TOutput();
                
                return converter(input);
            }
        }
    }
    
    // 泛型方法
    public static class GenericMethods
    {
        // 泛型方法 - 交換兩個變數的值
        public static void Swap<T>(ref T first, ref T second)
        {
            T temp = first;
            first = second;
            second = temp;
        }
        
        // 泛型方法 - 尋找陣列中的最大值
        public static T FindMax<T>(T[] array) where T : IComparable<T>
        {
            if (array == null || array.Length == 0)
                throw new ArgumentException("Array cannot be null or empty");
            
            T max = array[0];
            for (int i = 1; i < array.Length; i++)
            {
                if (array[i].CompareTo(max) > 0)
                    max = array[i];
            }
            return max;
        }
        
        // 泛型方法 - 檢查兩個值是否相等
        public static bool AreEqual<T>(T first, T second) where T : IEquatable<T>
        {
            if (first == null && second == null) return true;
            if (first == null || second == null) return false;
            return first.Equals(second);
        }
        
        // 泛型方法 - 建立指定型別的預設陣列
        public static T[] CreateArray<T>(int size, T defaultValue = default(T))
        {
            T[] array = new T[size];
            for (int i = 0; i < size; i++)
            {
                array[i] = defaultValue;
            }
            return array;
        }
    }
    
    class Program
    {
        static void Main()
        {
            Console.WriteLine("=== 泛型約束示範 ===");
            
            // 參考型別容器
            var stringContainer = new GenericConstraintExamples.ReferenceTypeContainer<string>();
            stringContainer.SetValue("Hello World");
            Console.WriteLine($"String container: {stringContainer.GetValue()}");
            
            // 值型別容器
            var intContainer = new GenericConstraintExamples.ValueTypeContainer<int>();
            intContainer.SetValue(42);
            Console.WriteLine($"Int container: {intContainer.GetValueOrDefault()}");
            
            // 工廠模式
            var circleFactory = new GenericConstraintExamples.Factory<Circle>();
            // var circle = circleFactory.CreateInstance(); // 這會失敗，因為Circle沒有預設建構函式
            
            // 形狀處理器
            var shapeProcessor = new GenericConstraintExamples.ShapeProcessor<Circle>();
            shapeProcessor.AddShape(new Circle(5.0));
            shapeProcessor.AddShape(new Circle(3.0));
            shapeProcessor.DrawAllShapes();
            Console.WriteLine($"Total area: {shapeProcessor.CalculateTotalArea():F2}");
            
            // 泛型方法示範
            Console.WriteLine("\n=== 泛型方法示範 ===");
            
            // 交換變數
            int a = 10, b = 20;
            Console.WriteLine($"Before swap: a={a}, b={b}");
            GenericMethods.Swap(ref a, ref b);
            Console.WriteLine($"After swap: a={a}, b={b}");
            
            // 尋找最大值
            int[] numbers = { 3, 7, 2, 9, 1, 8 };
            int maxNumber = GenericMethods.FindMax(numbers);
            Console.WriteLine($"Max number in array: {maxNumber}");
            
            string[] words = { "apple", "zebra", "banana", "cherry" };
            string maxWord = GenericMethods.FindMax(words);
            Console.WriteLine($"Max word in array: {maxWord}");
            
            // 建立陣列
            string[] defaultStrings = GenericMethods.CreateArray<string>(5, "Default");
            Console.WriteLine($"Default string array: [{string.Join(", ", defaultStrings)}]");
        }
    }
}
```

---

## 📦 第二節：集合和資料結構

### 2.1 基本集合類型

#### 2.1.1 List<T> - 動態陣列

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace CollectionsDemo
{
    public class Student
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int Age { get; set; }
        public string Major { get; set; }
        public double GPA { get; set; }
        
        public Student(int id, string name, int age, string major, double gpa)
        {
            Id = id;
            Name = name;
            Age = age;
            Major = major;
            GPA = gpa;
        }
        
        public override string ToString()
        {
            return $"Student[{Id}]: {Name}, {Age}歲, {Major}, GPA:{GPA:F2}";
        }
    }
    
    class ListExamples
    {
        static void Main()
        {
            Console.WriteLine("=== List<T> 基本操作 ===");
            BasicListOperations();
            
            Console.WriteLine("\n=== List<T> 進階操作 ===");
            AdvancedListOperations();
            
            Console.WriteLine("\n=== List<T> 效能考量 ===");
            ListPerformanceConsiderations();
        }
        
        static void BasicListOperations()
        {
            // 建立和初始化
            var numbers = new List<int> { 1, 2, 3, 4, 5 };
            var names = new List<string>();
            
            // 添加元素
            names.Add("Alice");
            names.Add("Bob");
            names.AddRange(new[] { "Charlie", "David", "Eve" });
            
            Console.WriteLine($"Numbers: [{string.Join(", ", numbers)}]");
            Console.WriteLine($"Names: [{string.Join(", ", names)}]");
            Console.WriteLine($"Count: {names.Count}, Capacity: {names.Capacity}");
            
            // 插入元素
            names.Insert(2, "Inserted Name");
            Console.WriteLine($"After insert: [{string.Join(", ", names)}]");
            
            // 存取元素
            Console.WriteLine($"First name: {names[0]}");
            Console.WriteLine($"Last name: {names[names.Count - 1]}");
            
            // 搜尋元素
            bool hasAlice = names.Contains("Alice");
            int indexOfBob = names.IndexOf("Bob");
            int lastIndexOfCharlie = names.LastIndexOf("Charlie");
            
            Console.WriteLine($"Contains 'Alice': {hasAlice}");
            Console.WriteLine($"Index of 'Bob': {indexOfBob}");
            Console.WriteLine($"Last index of 'Charlie': {lastIndexOfCharlie}");
            
            // 移除元素
            names.Remove("Bob"); // 移除第一個符合的元素
            names.RemoveAt(0);   // 移除指定索引的元素
            names.RemoveRange(1, 2); // 移除範圍內的元素
            Console.WriteLine($"After removals: [{string.Join(", ", names)}]");
            
            // 清空列表
            numbers.Clear();
            Console.WriteLine($"Numbers after clear: Count = {numbers.Count}");
        }
        
        static void AdvancedListOperations()
        {
            // 建立學生列表
            var students = new List<Student>
            {
                new Student(1, "Alice Johnson", 20, "Computer Science", 3.8),
                new Student(2, "Bob Smith", 22, "Mathematics", 3.5),
                new Student(3, "Charlie Brown", 19, "Physics", 3.9),
                new Student(4, "Diana Prince", 21, "Computer Science", 3.7),
                new Student(5, "Eve Wilson", 20, "Mathematics", 3.6)
            };
            
            Console.WriteLine("所有學生:");
            students.ForEach(s => Console.WriteLine($"  {s}"));
            
            // 使用Predicate進行搜尋
            var csStudent = students.Find(s => s.Major == "Computer Science");
            Console.WriteLine($"\n第一個CS學生: {csStudent}");
            
            var allCsStudents = students.FindAll(s => s.Major == "Computer Science");
            Console.WriteLine($"\n所有CS學生 ({allCsStudents.Count}人):");
            allCsStudents.ForEach(s => Console.WriteLine($"  {s}"));
            
            // 檢查條件
            bool hasHighGPA = students.Exists(s => s.GPA > 3.8);
            bool allAdults = students.TrueForAll(s => s.Age >= 18);
            
            Console.WriteLine($"\n有高GPA學生 (>3.8): {hasHighGPA}");
            Console.WriteLine($"所有學生都成年: {allAdults}");
            
            // 排序
            var sortedByGPA = new List<Student>(students);
            sortedByGPA.Sort((s1, s2) => s2.GPA.CompareTo(s1.GPA)); // 降序排列
            
            Console.WriteLine("\n按GPA排序 (高到低):");
            sortedByGPA.ForEach(s => Console.WriteLine($"  {s}"));
            
            // 轉換
            var studentNames = students.ConvertAll(s => s.Name);
            Console.WriteLine($"\n學生姓名: [{string.Join(", ", studentNames)}]");
            
            // 複製和子列表
            var backup = new List<Student>(students); // 複製建構函式
            var firstThree = students.GetRange(0, 3);  // 取得子列表
            
            Console.WriteLine($"\n前三個學生:");
            firstThree.ForEach(s => Console.WriteLine($"  {s}"));
        }
        
        static void ListPerformanceConsiderations()
        {
            // 容量管理
            var list = new List<int>(1000); // 預設容量，避免重複擴展
            Console.WriteLine($"Initial capacity: {list.Capacity}");
            
            // 大量添加時的效能差異
            var sw = System.Diagnostics.Stopwatch.StartNew();
            
            // 不預設容量
            var list1 = new List<int>();
            for (int i = 0; i < 100000; i++)
            {
                list1.Add(i);
            }
            sw.Stop();
            Console.WriteLine($"Without initial capacity: {sw.ElapsedMilliseconds}ms");
            
            sw.Restart();
            
            // 預設容量
            var list2 = new List<int>(100000);
            for (int i = 0; i < 100000; i++)
            {
                list2.Add(i);
            }
            sw.Stop();
            Console.WriteLine($"With initial capacity: {sw.ElapsedMilliseconds}ms");
            
            // 記憶體使用
            Console.WriteLine($"List1 - Count: {list1.Count}, Capacity: {list1.Capacity}");
            Console.WriteLine($"List2 - Count: {list2.Count}, Capacity: {list2.Capacity}");
            
            // 壓縮容量
            list1.TrimExcess();
            Console.WriteLine($"List1 after TrimExcess - Count: {list1.Count}, Capacity: {list1.Capacity}");
        }
    }
}
```

#### 2.1.2 Dictionary<TKey, TValue> - 字典

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace DictionaryDemo
{
    public class Product
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public decimal Price { get; set; }
        public string Category { get; set; }
        public int Stock { get; set; }
        
        public Product(string id, string name, decimal price, string category, int stock)
        {
            Id = id;
            Name = name;
            Price = price;
            Category = category;
            Stock = stock;
        }
        
        public override string ToString()
        {
            return $"{Name} (${Price:F2}) - Stock: {Stock}";
        }
    }
    
    class DictionaryExamples
    {
        static void Main()
        {
            Console.WriteLine("=== Dictionary 基本操作 ===");
            BasicDictionaryOperations();
            
            Console.WriteLine("\n=== Dictionary 進階應用 ===");
            AdvancedDictionaryUsage();
            
            Console.WriteLine("\n=== Dictionary 效能特性 ===");
            DictionaryPerformance();
        }
        
        static void BasicDictionaryOperations()
        {
            // 建立和初始化
            var ages = new Dictionary<string, int>
            {
                ["Alice"] = 25,
                ["Bob"] = 30,
                ["Charlie"] = 28
            };
            
            // 另一種初始化方式
            var capitals = new Dictionary<string, string>
            {
                { "Taiwan", "Taipei" },
                { "Japan", "Tokyo" },
                { "Korea", "Seoul" }
            };
            
            Console.WriteLine("年齡字典:");
            foreach (var kvp in ages)
            {
                Console.WriteLine($"  {kvp.Key}: {kvp.Value}歲");
            }
            
            // 添加元素
            ages.Add("David", 32);
            ages["Eve"] = 27; // 如果鍵不存在則添加，存在則更新
            
            // 存取元素
            Console.WriteLine($"\nAlice的年齡: {ages["Alice"]}");
            
            // 安全存取
            if (ages.TryGetValue("Bob", out int bobAge))
            {
                Console.WriteLine($"Bob的年齡: {bobAge}");
            }
            
            if (ages.ContainsKey("Frank"))
            {
                Console.WriteLine($"Frank的年齡: {ages["Frank"]}");
            }
            else
            {
                Console.WriteLine("找不到Frank");
            }
            
            // 更新元素
            ages["Alice"] = 26; // 更新現有值
            Console.WriteLine($"Alice更新後的年齡: {ages["Alice"]}");
            
            // 移除元素
            bool removed = ages.Remove("Charlie");
            Console.WriteLine($"移除Charlie: {removed}");
            
            // 遍歷字典
            Console.WriteLine("\n最終年齡字典:");
            foreach (var name in ages.Keys)
            {
                Console.WriteLine($"  {name}: {ages[name]}歲");
            }
            
            // 清空字典
            Console.WriteLine($"清空前元素數量: {ages.Count}");
            ages.Clear();
            Console.WriteLine($"清空後元素數量: {ages.Count}");
        }
        
        static void AdvancedDictionaryUsage()
        {
            // 產品庫存管理系統
            var inventory = new Dictionary<string, Product>
            {
                ["P001"] = new Product("P001", "筆記型電腦", 25000, "Electronics", 50),
                ["P002"] = new Product("P002", "無線滑鼠", 800, "Electronics", 200),
                ["P003"] = new Product("P003", "辦公椅", 3500, "Furniture", 30),
                ["P004"] = new Product("P004", "咖啡機", 12000, "Appliances", 15)
            };
            
            Console.WriteLine("庫存產品:");
            foreach (var product in inventory.Values)
            {
                Console.WriteLine($"  {product.Id}: {product}");
            }
            
            // 按類別分組統計
            var categoryStats = new Dictionary<string, CategoryInfo>();
            
            foreach (var product in inventory.Values)
            {
                if (!categoryStats.ContainsKey(product.Category))
                {
                    categoryStats[product.Category] = new CategoryInfo();
                }
                
                categoryStats[product.Category].ProductCount++;
                categoryStats[product.Category].TotalValue += product.Price * product.Stock;
                categoryStats[product.Category].TotalStock += product.Stock;
            }
            
            Console.WriteLine("\n類別統計:");
            foreach (var kvp in categoryStats)
            {
                Console.WriteLine($"  {kvp.Key}: {kvp.Value}");
            }
            
            // 庫存警告系統
            var lowStockProducts = new Dictionary<string, Product>();
            const int lowStockThreshold = 20;
            
            foreach (var kvp in inventory)
            {
                if (kvp.Value.Stock < lowStockThreshold)
                {
                    lowStockProducts[kvp.Key] = kvp.Value;
                }
            }
            
            Console.WriteLine($"\n低庫存警告 (少於{lowStockThreshold}件):");
            foreach (var product in lowStockProducts.Values)
            {
                Console.WriteLine($"  ⚠️ {product}");
            }
            
            // 價格範圍查詢
            var expensiveProducts = inventory.Values
                .Where(p => p.Price > 10000)
                .ToDictionary(p => p.Id, p => p);
            
            Console.WriteLine("\n高價產品 (>$10,000):");
            foreach (var product in expensiveProducts.Values)
            {
                Console.WriteLine($"  💰 {product}");
            }
        }
        
        static void DictionaryPerformance()
        {
            const int itemCount = 1000000;
            var dict = new Dictionary<int, string>();
            var list = new List<KeyValuePair<int, string>>();
            
            // 填充資料
            for (int i = 0; i < itemCount; i++)
            {
                string value = $"Value_{i}";
                dict[i] = value;
                list.Add(new KeyValuePair<int, string>(i, value));
            }
            
            var sw = System.Diagnostics.Stopwatch.StartNew();
            
            // Dictionary查詢效能 - O(1)
            sw.Restart();
            for (int i = 0; i < 1000; i++)
            {
                var key = new Random().Next(itemCount);
                var value = dict[key];
            }
            sw.Stop();
            Console.WriteLine($"Dictionary查詢1000次: {sw.ElapsedMilliseconds}ms");
            
            // List查詢效能 - O(n)
            sw.Restart();
            for (int i = 0; i < 1000; i++)
            {
                var key = new Random().Next(itemCount);
                var value = list.FirstOrDefault(kvp => kvp.Key == key).Value;
            }
            sw.Stop();
            Console.WriteLine($"List查詢1000次: {sw.ElapsedMilliseconds}ms");
            
            // 記憶體使用比較
            Console.WriteLine($"Dictionary元素數量: {dict.Count}");
            Console.WriteLine($"List元素數量: {list.Count}");
        }
    }
    
    public class CategoryInfo
    {
        public int ProductCount { get; set; }
        public decimal TotalValue { get; set; }
        public int TotalStock { get; set; }
        
        public override string ToString()
        {
            return $"{ProductCount}項產品, 總值${TotalValue:F2}, 總庫存{TotalStock}件";
        }
    }
}
```

#### 2.1.3 其他重要集合類型

```csharp
using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;

namespace OtherCollections
{
    class CollectionComparison
    {
        static void Main()
        {
            Console.WriteLine("=== HashSet<T> - 唯一值集合 ===");
            HashSetExamples();

            Console.WriteLine("\n=== Queue<T> - 先進先出佇列 ===");
            QueueExamples();

            Console.WriteLine("\n=== Stack<T> - 後進先出堆疊 ===");
            StackExamples();

            Console.WriteLine("\n=== SortedSet<T> - 排序唯一集合 ===");
            SortedSetExamples();

            Console.WriteLine("\n=== ConcurrentQueue<T> - 執行緒安全佇列 ===");
            ConcurrentQueueExamples();
        }

        static void HashSetExamples()
        {
            var set = new HashSet<string> { "apple", "banana", "cherry" };
            set.Add("banana"); // 不會重複
            set.Add("date");
            Console.WriteLine($"HashSet內容: [{string.Join(", ", set)}]");
            Console.WriteLine($"包含banana? {set.Contains("banana")}");
            set.Remove("apple");
            Console.WriteLine($"移除apple後: [{string.Join(", ", set)}]");
        }

        static void QueueExamples()
        {
            var queue = new Queue<int>();
            queue.Enqueue(1);
            queue.Enqueue(2);
            queue.Enqueue(3);
            Console.WriteLine($"Queue內容: [{string.Join(", ", queue)}]");
            Console.WriteLine($"Dequeue: {queue.Dequeue()}");
            Console.WriteLine($"Peek: {queue.Peek()}");
            Console.WriteLine($"Queue剩餘: [{string.Join(", ", queue)}]");
        }

        static void StackExamples()
        {
            var stack = new Stack<string>();
            stack.Push("A");
            stack.Push("B");
            stack.Push("C");
            Console.WriteLine($"Stack內容: [{string.Join(", ", stack)}]");
            Console.WriteLine($"Pop: {stack.Pop()}");
            Console.WriteLine($"Peek: {stack.Peek()}");
            Console.WriteLine($"Stack剩餘: [{string.Join(", ", stack)}]");
        }

        static void SortedSetExamples()
        {
            var sortedSet = new SortedSet<int> { 5, 3, 8, 1, 3 };
            Console.WriteLine($"SortedSet內容: [{string.Join(", ", sortedSet)}]");
            sortedSet.Add(6);
            sortedSet.Remove(3);
            Console.WriteLine($"更新後: [{string.Join(", ", sortedSet)}]");
        }

        static void ConcurrentQueueExamples()
        {
            var cq = new ConcurrentQueue<string>();
            cq.Enqueue("X");
            cq.Enqueue("Y");
            cq.Enqueue("Z");
            if (cq.TryDequeue(out var result))
                Console.WriteLine($"ConcurrentQueue Dequeue: {result}");
            Console.WriteLine($"剩餘: [{string.Join(", ", cq)}]");
        }
    }
}
```

**常見集合類型比較：**

| 類型                | 特性                     | 適用情境                     |
|---------------------|--------------------------|------------------------------|
| List<T>             | 有序、可重複             | 一般清單、索引存取           |
| Dictionary<TKey,TValue> | 鍵值對、快速查詢     | 需要依Key查詢、映射           |
| HashSet<T>          | 唯一、無序               | 去除重複、集合運算           |
| Queue<T>            | 先進先出                 | 排隊、任務處理               |
| Stack<T>            | 後進先出                 | 邏輯堆疊、回溯               |
| SortedSet<T>        | 唯一、自動排序           | 唯一且需排序的集合           |
| ConcurrentQueue<T>  | 執行緒安全FIFO           | 多執行緒任務排程             |

---

## 📖 第三節：LINQ查詢語法與資料處理

（請於下週教材深入學習LINQ語法與應用）

---

## 📝 本週總結與學習建議

本週我們深入探討了C#泛型、集合類型與其應用，重點如下：

1. **泛型程式設計**：提升型別安全、效能與程式碼重用性。
2. **各類集合**：根據需求選擇合適的集合型別，提升資料處理效率。
3. **集合操作技巧**：熟悉常用API與效能考量。

**學習建議：**
- 多練習各種集合的宣告、操作與遍歷。
- 嘗試設計小型資料結構應用（如：學生名單、商品庫存、任務排程）。
- 比較不同集合在查詢、插入、刪除上的效能差異。
- 預習LINQ語法，為下週資料查詢與轉換做準備。

---

*本教材版權所有，僅供學習使用。如有疑問，請聯繫課程講師。*