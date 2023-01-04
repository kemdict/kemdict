<script>
  import Header from "$lib/Header.svelte";
  import SearchBar from "$lib/SearchBar.svelte";
  // mdsvex passes the frontmatter key-value pairs as named arguments
  // to this component.
  export let title;
  export let date;
  export let desc;
</script>

<svelte:head>
  {#if title}
    <title>{title}</title>
    <meta property="og:title" content={title} />
    <meta property="twitter:title" content={title} />
  {/if}
  {#if desc}
    <meta name="description" content={desc} />
    <meta property="og:description" content={desc} />
    <meta property="twitter:description" content={desc} />
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
