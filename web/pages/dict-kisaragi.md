<script>
  import kisaragi_dict from "$lib/kisaragi_dict.json"
  import RecentWordList from "$lib/components/RecentWordList.svelte"
</script>

<svelte:head>
  <title>字典 | 如月的現代台灣華語補足典</title>
</svelte:head>

# 字典：如月的現代台灣華語補足典

我記錄台灣華語有在使用但沒被收錄的詞的地方。

不是只有流行語。

我認為字典與實際使用狀況不符時，錯的是不正視現實的字典，不是說話的人。見：「骰子」、「收件匣」、「丼」、「拖曳」。

我很感謝教育部各詞典編撰後公開供任何人使用。教育部詞典的編撰者們做的是我無法想像自己能有能力做到的事。雖說如此，我還是認為教育部詞典的編撰方針與現實脫節了。在大多人都不會看得懂「色子」的情況下堅持「骰子」正讀法是「ㄊㄡˊ ㄗ˙」；電郵系統出現至今數十年依然沒有收錄「寄件匣」、「收件匣」等等，我認為這是不合理的。

目前共有 {kisaragi_dict.length} 條：

<RecentWordList />
