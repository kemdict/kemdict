<script>
  import RecentWordList from "$lib/RecentWordList.svelte";
  import SearchBar from "$lib/SearchBar.svelte";
  import DarkModeToggle from "$lib/DarkModeToggle.svelte";
  import SplitLayout from "$lib/SplitLayout.svelte";
  import Out from "$lib/Out.svelte";
  const title = "Kemdict 國語整合典";
  const sitename = "Kemdict 國語整合典";
  const description =
    "《Kemdict 國語整合典》整合多部辭典，提供一次搜尋的功能。同時包含另一個我拿來記錄教育部沒有收錄的詞的字典。";
</script>

<svelte:head>
  <meta property="og:type" content="website" />
  <meta name="description" content={description} />
  <meta property="og:description" content={description} />
  <meta name="twitter:card" content="summary" />
  <title>{title}</title>
  <meta property="og:title" content={title} />
  <meta property="og:sitename" content={sitename} />
</svelte:head>

<SplitLayout wantHeader={false}>
  <div slot="left">
    <div class="sm:fixed">
      <h1 class="mt-6 text-xl font-bold max-sm:mb-4">
        <img
          decoding="sync"
          alt="logo"
          class="h-[1rem] w-[1rem] brightness-0 dark:brightness-100 max-sm:inline sm:h-[10rem] sm:w-[10rem]"
          src="/logo-white.png"
        />
        Kemdict 國語整合典 (β)
      </h1>
      <DarkModeToggle />
    </div>
  </div>
  <div class="prose" slot="right">
    <h2>前言</h2>
    <ul class="ml-1 list-none p-0">
      <li>
        <p><a href="about">關於</a></p>
      </li>
      <li>
        <p><a href="changelog">版本紀錄</a></p>
      </li>
      <li>
        <p><a href="thanks">致謝與授權</a></p>
      </li>
    </ul>
    <h2>搜尋</h2>
    <div class="ml-1">
      <SearchBar />
    </div>
    <h2>最近新增的詞</h2>
    <RecentWordList limit={12} />
    <h2>收錄字典</h2>
    <ul class="ml-1 list-none p-0">
      <li>
        <p><a href="dict-kisaragi">如月的現代台灣華語補足典</a></p>
      </li>
      <li>
        <p><a href="dict-itaigi">iTaigi 華台對照典</a></p>
      </li>
      <li>
        <p>
          <Out href="https://mhi.moe.edu.tw/infoList.jsp?ID=2&ID2=1705"
            >台灣白話基礎語句</Out
          >
        </p>
      </li>
      <li>
        <p><a href="dict-taijittoasutian">台日大辭典台語譯本 (1932)</a></p>
      </li>
      <li>
        <p><a href="dict-moe">教育部重編國語辭典修訂本</a></p>
      </li>
      <li>
        <p><a href="dict-moe">教育部國語辭典簡編本</a></p>
      </li>
      <li>
        <p><a href="dict-moe">教育部臺灣閩南語常用詞辭典</a></p>
      </li>
      <li>
        <p><a href="dict-moe">教育部臺灣客家語常用詞辭典</a></p>
      </li>
      <li>
        <p><a href="dict-moe">教育部成語典</a></p>
      </li>
    </ul>
  </div>
</SplitLayout>
