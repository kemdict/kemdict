import { List, Appbar } from "react-native-paper";
import useSWR from "swr";
import { FlatList, Text, View } from "react-native";
import { getHeteronyms } from ":/db.ts";

export default function Search() {
  const { data, isLoading } = useSWR("dummy", async () => {
    const [matchingDicts, hets, dictCountObj] = await getHeteronyms("水");
    return hets;
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
          data={hets}
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
