import {
  Container,
  List,
  ListItemIcon,
  ListItemText,
  ListItemButton,
  Typography,
} from "@mui/material";
import SearchAppBar from "./SearchAppBar.tsx";
import DescriptionIcon from "@mui/icons-material/Description";

export default function IndexPage({ title }: { title: string }) {
  return (
    <>
      <SearchAppBar title={title} />
      <Container>
        <Typography sx={{ mt: 2, mb: 0 }} variant="h6" component="div">
          索引
        </Typography>
        <List dense={false}>
          <ListItemButton>
            <ListItemIcon>
              <DescriptionIcon />
            </ListItemIcon>
            <ListItemText primary="首字索引" />
          </ListItemButton>
          <ListItemButton>
            <ListItemIcon>
              <DescriptionIcon />
            </ListItemIcon>
            <ListItemText primary="部首索引" />
          </ListItemButton>
          <ListItemButton>
            <ListItemIcon>
              <DescriptionIcon />
            </ListItemIcon>
            <ListItemText primary="新詞列表" />
          </ListItemButton>
        </List>
      </Container>
    </>
  );
}
