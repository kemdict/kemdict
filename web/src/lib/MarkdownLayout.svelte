<script>
  import Header from "$lib/components/Header.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  // mdsvex passes the frontmatter key-value pairs as named arguments
  // to this component.
  export let title;
</script>

<svelte:head>
  <title>{title}</title>
</svelte:head>
<Header>
  <SearchBar />
</Header>

<div class="prose">
  <slot />
</div>
