<script>
  // Thank you https://navillus.dev/blog/svelte-class-props/
  // For teaching me this syntax
  let klass = "";
  export { klass as class };
  export let initialInput = undefined;
  export let initialMatchSelection = undefined;
  import SearchBar from "$lib/SearchBar.svelte";
  import Header from "$lib/Header.svelte";
</script>

<Header>
  <SearchBar {initialInput} {initialMatchSelection} />
</Header>

<div class="{klass} max-w-2xl mx-auto">
  <slot />
</div>
