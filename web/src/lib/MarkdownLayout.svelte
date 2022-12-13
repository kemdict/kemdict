<script>
  import Header from "$lib/components/Header.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  // mdsvex passes the frontmatter key-value pairs as named arguments
  // to this component.
  export let title;
  export let date;
</script>

<svelte:head>
  {#if title}
    <title>{title}</title>
  {/if}
  <meta property="og:type" content="article" />
  {#if date}
    <meta property="og:article:modified_time" content={date} />
  {/if}
</svelte:head>
<Header>
  <SearchBar />
</Header>

<div class="prose">
  <slot />
</div>
