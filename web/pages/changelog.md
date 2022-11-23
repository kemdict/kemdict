<svelte:head>
  <title>版本紀錄</title>
</svelte:head>

# 版本紀錄

## 0.7.3 (unreleased)

- 修正搜尋頁面使用 `?s=` 而非如同其他搜尋引擎一樣的 `?q=`。
  我想要跟搜尋引擎一樣但弄錯了。
- 修正如「擺布」等某些詞的定義編號會跑掉的問題
- 在單字頁面中預先填入搜尋列

## 0.7.2 (2022-11-22)

- 新增搜尋頁面，取代下拉式搜尋元件
- 取代下拉式搜尋元件後不再需要在客戶端載入完整單字列表（快 18 萬個字、大概 5MB 的 JSON），因此頁面載入速度有改善
- 在找不到單字的頁面也加入搜尋列

## 0.7.1 (2022-11-20)

- 希望消除新詞列表閃一下的問題
- 修正 [dict-kisaragi](/dict-kisaragi) 頁面中文字列表前方有多餘的項目符號
- 新增暗色模式

## 0.7.0 (2022-11-18)

- 移植到 SvelteKit 上
- 修改文字頁面辭典標頭，不使用折疊按鈕
- 新網站圖示 (favicon)

## 0.6.1 (2022-11-14)

- 用 Overpass 當英數字型
- 修正定義列表數字超過 10 個的時候會被切掉
- 列出客家語常用詞辭典的所有讀音
- 國語辭典簡編版：顯示近義、反義；換一種方式顯示「例」、「英」等等標記
- 更新版本紀錄頁風格、文字頁面外部搜尋連結風格

## 0.6.0 (2022-11-14)

- 加入《臺灣客家語常用詞辭典》的內容
- 文字頁面上先顯示國語辭典簡編版的定義
- kisaragi-dict 的注音：像教育部辭典一樣一聲不寫聲調

## 0.5.3 (2022-11-11)

- 修正手機上新增至主畫面後，開啟時狀態列是全白的
- 使用 Esc 隱藏搜尋列
- 讓所有 URL 是小寫的

## 0.5.2 (2022-11-09)

- 首頁顯示 12 個新詞
- 更新原始碼連結
- 新增 logo
- （內部）試著製作 SvelteKit 版本

## 0.5.1 (2022-11-08)

- 修正只有讀音沒有定義的詞的顯示
- 修正 404 頁面元素卡在螢幕邊緣

## 0.5.0 (2022-11-07)

- 新增 metadata
- 新的主頁面排版
- 新增「最近新增的詞」區塊
- 新增字詞頁面折疊各辭典定義的功能
- 修正新的 404 頁面

## 0.4.0 (2022-11-06)

- 字詞頁面新增連結到樂詞網（學術詞彙資料庫）
- 更新 404 頁面
- 改善搜尋：兩端空格不會影響搜尋

## 0.3.0 (2022-11-05)

- 修正多音排序不正確的問題

## 0.2.0 (2022-11-04)

- 搜尋列顯示相符項目數量
- 搜尋列以固定方式排序相符項目

## 0.1.0 (2022-11-03)

- 經過一段試作時期後大致上能用的初始版本