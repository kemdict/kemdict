<script lang="ts">
  interface Props {
    id?: string | undefined;
    children?: import('svelte').Snippet;
  }

  let { id = undefined, children }: Props = $props();
  let Pn: HTMLDivElement;
</script>

<div bind:this={Pn} onclick={() => navigator.clipboard.writeText(Pn.innerText)} class="my-2 text-gray-800 dark:text-gray-200 text-lg" {id}>{@render children?.()}</div>
