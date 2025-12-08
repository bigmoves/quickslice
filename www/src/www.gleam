/// Docs site static site generator
import gleam/io
import lustre/ssg
import simplifile
import www/config.{type DocPage}
import www/loader
import www/og
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
      let all_pages = [first, ..rest]

      // Add first route to get HasStaticRoutes type, then add remaining
      let cfg =
        ssg.new(config.out_dir)
        |> ssg.add_static_dir(config.static_dir)
        |> ssg.add_static_route(first.path, page.render(first, all_pages))
        |> add_routes(rest, all_pages)
        |> ssg.use_index_routes

      case ssg.build(cfg) {
        Ok(_) -> {
          io.println("Build succeeded! Output: " <> config.out_dir)

          // Create og directory and generate single OG image
          let _ = simplifile.create_directory_all("./priv/og")
          generate_og_image()
        }
        Error(_) -> io.println("Build failed!")
      }
    }
  }
}

fn generate_og_image() -> Nil {
  case og.render() {
    Ok(bytes) -> {
      let path = og.output_path()
      case simplifile.write_bits(path, bytes) {
        Ok(_) -> io.println("Generated: " <> path)
        Error(_) -> io.println("Failed to write: " <> path)
      }
    }
    Error(_) -> io.println("Failed to render OG image")
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
