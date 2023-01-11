<script>
  // Thank you https://navillus.dev/blog/svelte-class-props/
  // For teaching me this syntax
  let klass = "";
  export { klass as class };
  export let initialInput = undefined;
  export let initialMatchSelection = undefined;
  import Header from "$lib/Header.svelte";
</script>

<Header search={true} {initialInput} {initialMatchSelection} />

<div class="{klass} max-w-2xl mx-auto">
  <slot />
</div>
