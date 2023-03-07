<script lang="ts">
  export let title: string;
  export let elements = [];
  export let template = "$1";
  import { format } from "$src/common";
</script>

<h2 class="mb-2">{title}</h2>
<ul class="flex list-none flex-wrap gap-x-2 p-0">
  {#each elements as element}
    <li class="w-4">
      <a href={format(template, element.title || element)}
        >{element.title || element}</a
      >
    </li>
  {/each}
</ul>
