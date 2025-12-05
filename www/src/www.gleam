/// Docs site static site generator
import gleam/io
import lustre/ssg
import www/config.{type DocPage}
import www/loader
import www/page

pub fn main() -> Nil {
  case loader.load_all() {
    Error(err) -> {
      io.println("Error loading docs: " <> err)
    }
    Ok([]) -> {
      io.println("No pages to build")
    }
    Ok([first, ..rest]) -> {
      // Add first route to get HasStaticRoutes type, then add remaining
      let cfg =
        ssg.new(config.out_dir)
        |> ssg.add_static_dir(config.static_dir)
        |> ssg.add_static_route(first.path, page.render(first, [first, ..rest]))
        |> add_routes(rest, [first, ..rest])
        |> ssg.use_index_routes

      case ssg.build(cfg) {
        Ok(_) -> io.println("Build succeeded! Output: " <> config.out_dir)
        Error(_) -> io.println("Build failed!")
      }
    }
  }
}

fn add_routes(cfg, remaining: List(DocPage), all_pages: List(DocPage)) {
  case remaining {
    [] -> cfg
    [p, ..rest] -> {
      cfg
      |> ssg.add_static_route(p.path, page.render(p, all_pages))
      |> add_routes(rest, all_pages)
    }
  }
}
