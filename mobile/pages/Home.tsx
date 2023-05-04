import { List, Appbar } from "react-native-paper";
import useSWR from "swr";
import { FlatList, Text, View } from "react-native";
import { readDB } from "../db.ts";

export default function Home() {
  const { data, isLoading } = useSWR("dummy", async () => {
    const db = await readDB();
    return await new Promise((resolve) => {
      db.transaction((tx) => {
        tx.executeSql(
          `select * from "heteronyms" limit 10;`,
          [],
          (_, resultSet) => {
            resolve(resultSet.rows._array);
          },
          (_, _err) => {
            resolve([{ title: "error" }]);
          }
        );
      });
    });
  });
  return (
    <>
      {isLoading ? (
        <View>
          <Text>Loading</Text>
        </View>
      ) : (
        <FlatList
          ListHeaderComponent={
            <Appbar.Header>
              <Appbar.Content title="Kemdict" />
            </Appbar.Header>
          }
          data={data}
          renderItem={({ item }) => (
            <View>
              <List.Item
                title={item.title}
                description={JSON.stringify(item)}
              />
            </View>
          )}
          keyExtractor={(item) => item.title}
        ></FlatList>
      )}
    </>
  );
}
