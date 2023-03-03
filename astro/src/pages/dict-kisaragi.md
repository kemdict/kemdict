---
title: 字典 | 如月的現代台灣華語補足典
---

<script>
  export let data;
  import WordList from "$lib/WordList.svelte"
</script>

# 字典 — 如月的現代台灣華語補足典

我記錄台灣華語有在使用但沒被收錄的詞的地方。

目前共有 {data.titles.length} 條：

<WordList words={data.titles} />
