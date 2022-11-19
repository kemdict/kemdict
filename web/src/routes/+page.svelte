<script>
  import RecentWordList from "$lib/components/RecentWordList.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  import DarkModeToggle from "$lib/components/DarkModeToggle.svelte";
  const title = "kemdict";
  const sitename = "kemdict";
  const description =
    "Kemdict 整合了多個教育部的辭典，提供一次搜尋的功能。同時包含另一個我拿來記錄教育部沒有收錄的詞的字典。";
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

<div class="index relative">
  <div class="prose mt-12 md:ml-[14rem]">
    <div class="max-md:mb-6 md:absolute md:top-4 md:left-4">
      <div class="md:fixed md:w-[10rem]">
        <img class="h-[10rem] w-[10rem]" src="/logo.png" />
        <h1 class="font-bold text-3xl">kemdict (β)</h1>
        <DarkModeToggle />
      </div>
    </div>
    <h2>前言</h2>
    <ul>
      <li>
        <p><a href="about">關於 Kemdict</a></p>
        <p><a href="changelog">版本紀錄</a></p>
        <p><a href="thanks">致謝與授權</a></p>
      </li>
    </ul>
    <h2>搜尋</h2>
    <SearchBar />
    <h2>最近新增的詞</h2>
    <RecentWordList limit={12} />
    <h2>收錄字典</h2>
    <ul>
      <li>
        <p><a href="dict-kisaragi">如月的現代台灣華語補足典</a></p>
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
</div>
