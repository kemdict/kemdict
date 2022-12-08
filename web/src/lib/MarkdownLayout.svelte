<script>
  import Header from "$lib/components/Header.svelte";
  // mdsvex passes the frontmatter key-value pairs as named arguments
  // to this component.
  export let title;
</script>

<svelte:head>
  <title>{title}</title>
</svelte:head>
<Header />
<div class="prose">
  <slot />
</div>
