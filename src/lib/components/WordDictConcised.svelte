<script>
  export let entry;
  export let title;
  import { spc } from "$lib/common";
</script>

{#each entry.heteronyms as het}
  <h1>{title}</h1>
  {#if het.bopomofo}
    <p>讀音：<span>{spc(het.bopomofo)}</span></p>
  {/if}
  {#if het.definition}
    <ol>
      {#each het.definition.split("\n") as d}
        <li><p class="def">{d.replace(/^\d+\./, "")}</p></li>
      {/each}
    </ol>
  {/if}
{/each}
