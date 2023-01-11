<script>
  export let words;
</script>

<div class="not-prose">
  <ul class="flex flex-wrap">
    {#each words as word}
      <li class="mr-2 py-2">
        <a class="wordlink" href="/word/{word}">{word}</a>
      </li>
    {/each}
  </ul>
</div>
