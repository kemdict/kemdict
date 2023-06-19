<script lang="ts">
  export let search: boolean;
  export let words: string[];

  let input = "";

  function highlight(word: string, input: string) {
    if (input === "") return word;
    return word
      .split(input)
      .join(`<span class="bg-yellow-400 dark:bg-yellow-800">${input}</span>`);
  }
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
            {@html highlight(word, input)}
          </a>
        </li>
      {/if}
    {/each}
  </ul>
</div>
