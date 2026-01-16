<script lang="ts">
  import type { Heteronym } from "common";
  const {
    heteronyms,
  }: {
    heteronyms: Array<
      Heteronym<{
        id: string;
        zh: string;
        def: string;
        words: Array<{ han: string; tl: string }>;
      }>
    >;
  } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
</script>

{#each heteronyms as het}
  <div class="flex w-full items-center">
    <h1>{het.title}</h1>
  </div>
  <p class="def">
    {het.props.def}
  </p>
  {JSON.stringify(het.props)}
{/each}
