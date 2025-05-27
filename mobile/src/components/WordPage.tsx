import {
  Container,
  List,
  ListItemIcon,
  ListItemText,
  ListItemButton,
  Typography,
} from "@mui/material";
import SearchAppBar from "./SearchAppBar.tsx";
import { NuqsAdapter } from "nuqs/adapters/react";
import { useQueryState } from "nuqs";

function WordContent() {
  const [word] = useQueryState("word");
  return (
    <Typography sx={{ mt: 2, mb: 0 }} variant="h6" component="div">
      {word}
    </Typography>
  );
}

export default function WordPage() {
  return (
    <NuqsAdapter>
      <SearchAppBar />
      <Container>
        <WordContent></WordContent>
      </Container>
    </NuqsAdapter>
  );
}
