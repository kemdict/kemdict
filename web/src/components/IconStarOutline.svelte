<script lang="ts">
  const { active = false } = $props();
</script>

<!-- ionicons: star-outline -->
<svg xmlns="http://www.w3.org/2000/svg" class="ionicon" viewBox="0 0 512 512"
  ><path
    d="M480 208H308L256 48l-52 160H32l140 96-54 160 138-100 138 100-54-160z"
    fill="none"
    stroke="currentColor"
    stroke-linejoin="round"
    stroke-width={active ? 50 : 32}
  /></svg
>
