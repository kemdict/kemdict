<script>
  import DarkModeToggle from "$lib/DarkModeToggle.svelte";
</script>

<div class="mb-8">
  <h1 class="my-2 justify-between flex items-baseline">
    <a class="font-bold hover:underline" href="/">Kemdict 國語整合典 (β)</a>
    <DarkModeToggle />
  </h1>
  <slot />
</div>
