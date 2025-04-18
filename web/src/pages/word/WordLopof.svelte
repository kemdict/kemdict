<script lang="ts">
  const { heteronyms } = $props();
  import type { Heteronym } from "common";
</script>

{#each heteronyms as het}
  <div class="flex w-full items-center">
    <h1>{het.title}</h1>
  </div>
  {het.props.scientificName}
{/each}
