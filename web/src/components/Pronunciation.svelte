<script lang="ts">
  export let id: string | undefined = undefined;
</script>

<div class="text-gray-700 dark:text-gray-300" {id}><slot /></div>
