# atomとgithub

テキストエディタ Atom とgithubとを連携することができる。

Atomはgithubで開発されたテキストエディタであり、連携ツールもある。

# atomのインストール

インストールサイト https://atom.io/ からダウンロードしてインストールするだけ。

Windows, Mac, Linuxの３OSに対応している。

アプリケーションの日本語化をするには、atomのパッケージ japanese-menu をインストールすればよい。
こちらのサイト https://eng-entrance.com/atom-editor が参考になる。

# 文字コード

OSをまたがって使うときには文字コードも揃えたい。
Atomの場合、ファイルを開くと、アプリケーションの右下にエンコーディング指定ができるのでWindosｓの場合にもUTF-8を指定しておくと良いだろう。
そうすることで３OSで文字コードを揃えることができる。

# githubとの連携１

まずは、Atomを使いながら、自分のgithubアカウントとのつながりを作らないといけない。
それには、Atom上で、自身のgithubアカウントをONにする必要がある。
その手順についてはこちら https://www.sejuku.net/blog/73327 を参照。
「トークン」と呼ばれるパスワードのようなものを入手してそれを入力することで連携が取れる。

# githubとの連携２

githubに連携したいrepositoryを作る。
git clone して、ローカルにgithub のrepositoryに対応するディレクトリを作る。
Atomでそのディレクトリを指定して新しいプロジェクトフォルダとして追加する。

あとは、そのフォルダの中のファイルを修正したり、新しいファイルを作ったりする。

ローカルでの変更を保存すると、git タブのUnstaged Changesのところに、変更を持つファイルが現れる。

Unstaged Changesの右上のStage Allをクリックすると、そのファイルがStaged Changesに移動する。

この状態で、Commit messageにメッセージを入れ、その下にあるCommit to masterをクリックすることで、その下にCommitするべき内容が現れる。そして、Atomアプリケーションの末尾（の通常はFetchと書かれている部分）にPushという文字が現れるので、それをクリックすると、githubに変化が反映する。

# Windowsのpython環境

Windowsでpython環境を作るのには、anacondaで一通りを入れるのがよいが、普通に入れると、パスが通らなかったり、windows10だと、パスを後付けで通そうにも何がなんだかわからなかったりする。

ということで、こちら https://qiita.com/kumazawajiro/items/864a8bf99bc97eee10ea にある"choco install anaconda3 --params="/AddToPath:1" というコマンドでアナコンダをインストールし、パスも通してもらうことにする。

そのためにはchocolateyというものを入れる必要があるそうで、そのためには管理者権限でpowershellを使うのがよいらしい。こちら https://ryamada.hatenadiary.jp/entry/20190102/1546431857 にメモしておいた。
