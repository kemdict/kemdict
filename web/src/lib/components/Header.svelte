<script>
  import DarkModeToggle from "$lib/components/DarkModeToggle.svelte";
</script>

<div>
  <h1 class="my-2 justify-between flex items-baseline">
    <a data-sveltekit-reload class="link" href="/">kemdict (β)</a>
    <DarkModeToggle />
  </h1>
  <slot />
</div>
