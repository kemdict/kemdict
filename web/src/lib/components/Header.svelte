<script>
  import DarkModeToggle from "$lib/components/DarkModeToggle.svelte";
</script>

<div>
  <h1 class="my-2 justify-between flex items-baseline">
    <a class="font-bold hover:underline" href="/">Kemdict 國語整合典 (β)</a>
    <DarkModeToggle />
  </h1>
  <slot />
</div>
