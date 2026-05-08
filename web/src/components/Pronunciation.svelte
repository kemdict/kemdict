<script lang="ts">
  interface Props {
    id?: string | undefined;
    children?: import('svelte').Snippet;
  }

  let { id = undefined, children }: Props = $props();
</script>

<div onclick={(e: Event) => navigator.clipboard.writeText(e.target.innerText)} class="my-2 text-gray-800 dark:text-gray-200 text-lg" {id}>{@render children?.()}</div>
