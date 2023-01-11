<script>
  import { onMount, onDestroy } from "svelte";
  import { cubicOut } from "svelte/easing";
  import { tweened } from "svelte/motion";
  import { navigating } from "$app/stores";
  import { showLoadingAfterMS } from "$lib/common";

  const progress = tweened(0, {
    duration: 3000,
    delay: showLoadingAfterMS,
    easing: cubicOut,
  });

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
  <div class="progress h-1 bg-indigo-500" style="--width:{$progress * 100}%" />
</div>

<style>
  .progress {
    width: var(--width);
  }
</style>
