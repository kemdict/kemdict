<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { taigiTitle } from "$src/server/db";
  function process_def(def: string | undefined): string {
    if (def) {
      let x = "<ol>";
      for (const d of def.split(/\n/)) {
        x += `<li><p class="def">${d.replace(/\(\d+\)/, "")}</p></li>`;
      }
      x += "</ol>";
      return x;
    }
  }
  function splitExample(ex: string | undefined): string[] {
    if (!ex) return [];
    if (ex.match(/\(\d+\)/)) {
      return ex.replace(/\((\d+)\)/g, "($1) ").split("\n");
    } else {
      return ex.split("；");
    }
  }
</script>

{#each heteronyms as het}
  <h1>{taigiTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  {#if het.props.kipOthers}
    <div>其他：{het.props.kipOthers}</div>
  {/if}
  {#if het.props.def}
    <p class="my-2">{@html process_def(het.props.def)}</p>
    {#if het.props.example}
      <blockquote>
        <ul class="list-disc pl-2">
          {#each splitExample(het.props.example) as ex}
            <li>{ex}</li>
          {/each}
        </ul>
      </blockquote>
    {/if}
  {:else}
    <p class="my-2">{@html process_def(het.props.example)}</p>
  {/if}
{/each}
