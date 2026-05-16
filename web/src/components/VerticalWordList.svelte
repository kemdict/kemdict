<script lang="ts">
  export let etc: string | undefined = undefined;
  export let deleter: ((word: string) => void) | undefined = undefined;
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
  <ul class="md:flex md:flex-wrap md:gap-2">
    {#each words as word (word)}
      {#if !search || filterString === "" || word
          .toLowerCase()
          .includes(filterString.toLowerCase())}
        <li class="mb-1 flex shadow">
          <a class="fade btnColor flex-grow px-2 py-2" href="{prefix}{word}">
            {@html highlight(word, filterString)}
          </a>
          {#if deleter}
            <!-- ionicons close-outline -->
            <button
              title="刪除此項目"
              onclick={() => deleter(word)}
              class="variant-filled btn rounded-none px-2 py-2"
              ><svg
                class="h-5 w-5"
                xmlns="http://www.w3.org/2000/svg"
                width="512"
                height="512"
                viewBox="0 0 512 512"
                ><line
                  x1="368"
                  y1="368"
                  x2="144"
                  y2="144"
                  style="fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"
                /><line
                  x1="368"
                  y1="144"
                  x2="144"
                  y2="368"
                  style="fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"
                /></svg
              ></button
            >
          {/if}
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
