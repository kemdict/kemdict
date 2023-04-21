<script lang="ts">
  export let title: string | undefined;
  export let elements = [];
  export let template = "$1";
  import { format } from "$src/common";
</script>

<h2 class="mb-2 text-xl">
  {#if title}
    {title}
  {:else}
    其他
  {/if}
</h2>
<ul class="flex list-none flex-wrap gap-x-2 p-0">
  {#each elements as element}
    <li class="w-5 text-lg max-sm:my-1 max-sm:text-xl">
      <a
        class="inline-flex w-5"
        href={format(template, element.title || element.radical || element)}
        >{element.title || element.radical || element}</a
      >
    </li>
  {/each}
</ul>
