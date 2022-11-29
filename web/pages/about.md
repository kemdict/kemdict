<script>
import { version } from "$lib/common"
import SearchBar from "$lib/components/SearchBar.svelte";
</script>

<svelte:head>
<title>關於 Kemdict</title>
</svelte:head>

# 關於 Kemdict

《Kemdict 國語整合典》 提供對數本教育部辭典的一次搜尋。像是 [Weblio](https://weblio.jp/) 一樣，同一個詞會列出所有收錄字典的定義，並且每個定義有標明是哪個字典來的。

<SearchBar />

- 版本：[{version}](/changelog)
- 原始碼：<https://github.com/kemdict/kemdict/>

如果 Kemdict 有幫助到您，請考慮[贊助我一餐飯](https://www.buymeacoffee.com/kisaragihiu)。謝謝。
