<script>
  export let words;
</script>

<div class="not-prose">
  <ul class="flex flex-wrap">
    {#each words as word}
      <li class="py-2 mr-2">
        <a class="wordlink" href="/word/{word}">{word}</a>
      </li>
    {/each}
  </ul>
</div>
