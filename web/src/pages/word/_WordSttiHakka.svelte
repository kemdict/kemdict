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
        四縣詞彙: string;
        四縣音讀: string;
        南四縣詞彙: string;
        南四縣音讀: string;
        海陸詞彙: string;
        海陸音讀: string;
        大埔詞彙: string;
        大埔音讀: string;
        饒平詞彙: string;
        饒平音讀: string;
        饒平腔備註詞彙_卓蘭: string;
        饒平腔備註音讀_卓蘭: string;
        詔安詞彙: string;
        詔安音讀: string;
      }>
    >;
  } = $props();
  const keys = [
    "四縣詞彙",
    "四縣音讀",
    "南四縣詞彙",
    "南四縣音讀",
    "海陸詞彙",
    "海陸音讀",
    "大埔詞彙",
    "大埔音讀",
    "饒平詞彙",
    "饒平音讀",
    "饒平腔備註詞彙_卓蘭",
    "饒平腔備註音讀_卓蘭",
    "詔安詞彙",
    "詔安音讀",
  ] as const;
  import Property from "$src/components/Property.svelte";
</script>

{#each heteronyms as het}
  <div class="flex w-full items-center">
    <h1>{het.title}</h1>
  </div>
  <p class="def">
    {het.props.def}
  </p>
  {#each keys as key}
    <Property {key} value={het.props[key]?.trim().split("\n").join("、")}
    ></Property>
  {/each}
{/each}
