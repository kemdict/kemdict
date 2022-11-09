<script>
  import { navigating } from "$app/stores";
  import { tweened } from "svelte/motion";
  import { cubicOut } from "svelte/easing";
  import { onMount, onDestroy } from "svelte";
  const progress = tweened(0, { duration: 3000, easing: cubicOut });
  const unsubscribe = navigating.subscribe((value) => {
    if (!value) {
      progress.set(1, { duration: 1000 });
    }
  });
  onMount(() => {
    progress.set(0.7);
  });
  onDestroy(() => {
    unsubscribe();
  });
</script>

<div class="fixed top-0 left-0 right-0 h-1">
  <div class="h-1 bg-indigo-500 progress" style="--width:{$progress * 100}%" />
</div>

<style>
  .progress {
    width: var(--width);
  }
</style>
