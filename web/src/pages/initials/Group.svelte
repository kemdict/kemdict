<script lang="ts">
  export let title: string;
  export let elements = [];
</script>

<h2 class="mb-2">{title}</h2>
<ul class="flex list-none flex-wrap gap-x-2 p-0">
  {#each elements as element}
    <li class="w-4">
      <a href="/search?q={element.title || element}&m=prefix"
        >{element.title || element}</a
      >
    </li>
  {/each}
</ul>
