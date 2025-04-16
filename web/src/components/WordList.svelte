<script lang="ts">
  // TODO: sort by length; sort by codepoint; reset sort to default
  // (as stored in database - by date for kisaragi-dict and by (TODO)
  // backlinks ranking for backlinks)

  export let etc: string;
  export let words: string[];
  export let search: boolean = words.length > 20;
  /** Links link to /word/foo by default; this is the "/word/" part. */
  export let prefix = "/word/";

  let filterString = "";
  import LocalSearchInput from "$src/components/LocalSearchInput.svelte";

  function highlight(word: string, filterString: string) {
    if (filterString === "") return word;
    return word
      .split(filterString)
      .join(
        `<span class="bg-yellow-400 dark:bg-yellow-800">${filterString}</span>`,
      );
  }
</script>

<div class="not-prose">
  {#if search}
    <LocalSearchInput bind:filterString />
  {/if}
  <ul class="flex flex-wrap">
    {#each words as word (word)}
      {#if !search || filterString === "" || word
          .toLowerCase()
          .includes(filterString.toLowerCase())}
        <li class="mr-2 py-2">
          <a class="wordlink" href="{prefix}{word}">
            {@html highlight(word, filterString)}
          </a>
        </li>
      {/if}
    {/each}
    {#if etc}
      <li class="mr-2 py-2">
        <a class="wordlink" href={etc}> ... </a>
      </li>
    {/if}
  </ul>
</div>
