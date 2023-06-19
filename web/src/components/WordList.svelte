<script lang="ts">
  export let search: boolean;
  export let words: string[];

  // This is more reliable than binding value then setting input to
  // value in handlers. The bound value might update too late.
  let inputElem: HTMLInputElement;
  let filterString = "";
  // Paused on compositionstart (IME starts editing), unpaused on
  // compositionend. This allows the user to continue seeing the
  // unfiltered list while still typing in their IME.
  let updatePaused = false;

  function highlight(word: string, filterString: string) {
    if (filterString === "") return word;
    return word
      .split(filterString)
      .join(
        `<span class="bg-yellow-400 dark:bg-yellow-800">${filterString}</span>`
      );
  }
</script>

<div class="not-prose">
  {#if search}
    <input
      type="search"
      autocomplete="off"
      placeholder="搜尋…"
      class="k-input mb-1 h-10 w-full"
      bind:this={inputElem}
      on:input={() => {
        if (inputElem.value === "" || !updatePaused) {
          filterString = inputElem.value;
        }
      }}
      on:compositionstart={() => (updatePaused = true)}
      on:compositionend={() => (updatePaused = false)}
    />
  {/if}
  <ul class="flex flex-wrap">
    {#each words as word (word)}
      {#if !search || filterString === "" || word.includes(filterString)}
        <li class="mr-2 py-2">
          <a class="wordlink" href="/word/{word}">
            {@html highlight(word, filterString)}
          </a>
        </li>
      {/if}
    {/each}
  </ul>
</div>
