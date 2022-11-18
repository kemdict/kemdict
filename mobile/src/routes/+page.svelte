<script>
  import RecentWordList from "$lib/components/RecentWordList.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  const title = "kemdict";
  const sitename = "sitename";
  const description = "Dummy description.";
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
        <img
          class="max-md:inline max-md:align-baseline max-md:max-h-[1.5rem] max-md:max-w-[1.5rem] md:min-w-[10rem] md:min-h-[10rem]"
          src="/logo.png"
          alt="logo"
        />
        <h1 class="max-md:inline font-bold text-3xl">kemdict (β)</h1>
      </div>
    </div>
    <h2>前言</h2>
    <ul>
      <li>
        <p><a href="about">關於 Kemdict</a></p>
        <p><a href="changelog">版本紀錄</a></p>
        <p><a href="thanks">致謝</a></p>
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
        <p><a href="dict-moe">教育部成語典</a></p>
      </li>
    </ul>
  </div>
</div>
