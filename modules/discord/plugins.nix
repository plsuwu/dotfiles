{
  theme ? "tokyo-night",
}:
let
  repo = "shikijs/textmate-grammars-themes";
  commit = "2d87559c7601a928b9f7e0f0dda243d2fb6d4499";
  shikiRepoTheme =
    name:
    "https://raw.githubusercontent.com/${repo}/${commit}/packages/tm-themes/themes/${name}.json";
in
{
  enabled = [
    "FriendsSince"
    "NoTypingAnimation"
    "ReadAllNotificationsButton"
    "MessageLogger"
    "ShowMeYourName"
    "NoReplyMention"
    "ShowHiddenChannels"
    "RelationshipNotifier"
  ];

  settings = {
    ShowMeYourName = {
      mode = "nick-user";
      inReplies = true;
    };

    NoReplyMention = {
      inverseShiftReply = true;
    };

    ShowHiddenChannels = {
      showMode = 1;
    };

    RelationshipNotifier = {
      notices = true;
    };
  };
}
