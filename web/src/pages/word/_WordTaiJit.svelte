<script lang="ts">
  import type { Heteronym } from "common";
  const { heteronyms } = $props() as { heteronyms: Heteronym[] };
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { taigiTitle } from "common";
  function splitDef(def: string | undefined): [number, string][] | undefined {
    if (def) {
      return def.split(/\n/).map((d, i) => [i + 1, d.replace(/\(\d+\)/, "")]);
    }
  }
  // Split examples into entries with information of which definition it is for
  function splitExample(exs: string | undefined): [number, string][] {
    if (!exs) return [];
    if (exs.match(/\(\d+\)/)) {
      return exs.split("\n").map((ex) => {
        const forWhich = ex.match(/\((\d+)\)/)?.[1] || "0";
        return [parseInt(forWhich), ex.replace(/\((\d+)\)/g, "")];
      });
    } else {
      return exs.split("；").map((ex) => [0, ex]);
    }
  }
</script>

{#each heteronyms as het}
  <h1>{taigiTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  <Property key="其他" value={het.props.kipOthers} />
  {#if het.props.def}
    {@const examples = splitExample(het.props.example)}
    <ol class="my-2">
      {#each splitDef(het.props.def) as [n, def]}
        <li>
          <p class="def">{@html def}</p>
          {#each examples as [forWhich, ex]}
            {#if forWhich === n}
              <blockquote>{ex}</blockquote>
            {/if}
          {/each}
        </li>
      {/each}
    </ol>
    {#each examples as [forWhich, ex]}
      {#if forWhich === 0}
        <blockquote>{ex}</blockquote>
      {/if}
    {/each}
  {:else}
    <!-- Some entries have definition fields that ended up in the example field instead -->
    <ol class="my-2">
      {#each splitDef(het.props.example) as [_n, def]}
        <li>
          <p class="def">{@html def}</p>
        </li>
      {/each}
    </ol>
  {/if}
{/each}
