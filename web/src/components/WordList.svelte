<script lang="ts">
  export let search: boolean;
  export let words: string[];

  let input = "";
</script>

<div class="not-prose">
  {#if search}
    <input
      type="search"
      autocomplete="off"
      placeholder="篩選…"
      class="text-input"
      bind:value={input}
    />
  {/if}
  <ul class="flex flex-wrap">
    {#each words as word (word)}
      {#if !search || word.includes(input)}
        <li class="mr-2 py-2">
          <a class="wordlink" href="/word/{word}">
            {word}
          </a>
        </li>
      {/if}
    {/each}
  </ul>
</div>
