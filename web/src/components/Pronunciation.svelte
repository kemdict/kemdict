<script lang="ts">
  interface Props {
    id?: string | undefined;
    children?: import('svelte').Snippet;
  }

  let { id = undefined, children }: Props = $props();
</script>

<button title="複製" onclick={(e) => navigator.clipboard.writeText(e.target.innerText)} class="hover:opacity-75 active:opacity-90 fade cursor-pointer my-2 text-gray-800 dark:text-gray-200 text-lg" {id}>{@render children?.()}</button>
