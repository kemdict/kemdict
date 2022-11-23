<script>
  // sure...
  export let limit = undefined;
  import kisaragi_dict from "$lib/kisaragi_dict.json";

  const words = kisaragi_dict.sort((a, b) => a.added < b.added).slice(0, limit);
</script>

<div class="not-prose">
  <ul class="flex flex-wrap">
    {#each words as word (word.title)}
      <li class="py-2 mr-2">
        <a data-sveltekit-reload class="wordlink" href="/word/{word.title}"
          >{word.title}</a
        >
      </li>
    {/each}
  </ul>
</div>
