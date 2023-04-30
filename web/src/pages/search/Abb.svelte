<script lang="ts">
  import WordPreview from "$src/components/WordPreview.svelte";
  import ListLink from "$src/components/ListLink.svelte";
  import { dictsByLang } from "$src/common";
  import type { Heteronym } from "$src/common";

  // id, name
  export let lang: [string, string];
  export let heteronyms: Heteronym[];
  export let currentParams: URLSearchParams;
  const previewCount = 5;

  const applicableHeteronyms = heteronyms.filter((het) =>
    dictsByLang[lang[0]].includes(het.from)
  );
</script>

<div class="mb-6">
  <h1 class="font-bold">{lang[1]} ({applicableHeteronyms.length})</h1>
  <ul>
    <WordPreview heteronyms={applicableHeteronyms.slice(0, previewCount)} />
    {#if applicableHeteronyms.length > previewCount}
      <ListLink href="/search/{lang[0]}?{currentParams.toString()}">
        <div class="text-center" slot="heading">
          共 {applicableHeteronyms.length} 項結果
        </div>
      </ListLink>
    {/if}
  </ul>
</div>
