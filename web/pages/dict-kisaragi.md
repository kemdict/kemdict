<script>
  import kisaragi_dict from "$lib/kisaragi_dict.json"
  import RecentWordList from "$lib/components/RecentWordList.svelte"
</script>

<svelte:head>
  <title>字典 | 如月的現代台灣華語補足典</title>
</svelte:head>

# 字典：如月的現代台灣華語補足典

我記錄台灣華語有在使用但沒被收錄的詞的地方。

目前共有 {kisaragi_dict.length} 條：

<RecentWordList />
